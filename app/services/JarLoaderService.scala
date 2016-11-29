package services

import java.net.{URL, URLClassLoader}
import java.util.jar.{JarEntry, JarFile}
import javax.inject.{Inject, Singleton}

import controllers.conversion.JsonConversionsProvider
import org.scalarules.dsl.nl.grammar.Berekening
import org.scalarules.utils.Glossary
import play.api.Configuration

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

/**
  * This service is tasked with loading JAR-files and scanning it for usable classes. It currently supports loading
  * Glossary and Berekening types.
  *
  * After initialization, this service provides access to the JARs which were attempted to load.
  *
  * @param configuration Play application configuration (injected)
  */
@Singleton
class JarLoaderService @Inject() (configuration: Configuration) {

  private val jarsToLoad: List[String] = configuration.getStringList("jars.load").map( _.asScala.toList ).getOrElse( List[String]() )
  private val triedJars: Map[String, Try[JarLoadingResults]] = jarsToLoad.map( jarLocation => (jarLocation, loadJar(jarLocation)) ).toMap

  val jars: Map[String, JarLoadingResults] = triedJars.collect {
    case (jarName: String, Success(loadedJar)) => (jarName, loadedJar)
  }

  val jarStatusses: Map[String, String] = triedJars.map {
    case (jarName: String, Success(_)) => (jarName, "Loaded")
    case (jarName: String, Failure(exception)) => (jarName, s"Failed to load: ${exception.getMessage.takeWhile( ch => ch != '{' )})}")
  }

  private def loadJar(location: String): Try[JarLoadingResults] = Try({
    val jarFile: JarFile = new JarFile(location)
    val jarClassEntries: List[JarEntry] = jarFile.entries().asScala
      .filter( je => !je.isDirectory && je.getName.endsWith(".class") ).toList

    val urls: Array[URL] = Array( new URL("jar:file:" + location + "!/") )
    val cl: URLClassLoader = URLClassLoader.newInstance(urls, this.getClass.getClassLoader)

    val mirror = runtimeMirror(cl)

    val triedGlossaries: List[Try[Glossary]] = scanAndLoadClasses(jarClassEntries, new GlossaryClassLoader, cl, mirror)
    val triedDerivations: List[Try[Berekening]] = scanAndLoadClasses(jarClassEntries, new DerivationClassLoader, cl, mirror)
    val triedJsonConversionsProviders: List[Try[JsonConversionsProvider]] = scanAndLoadClasses(jarClassEntries, new JsonConversionMapClassLoader, cl, mirror)

    JarLoadingResults(
      jarName = location,
      glossaries = triedGlossaries.collect { case Success(glossary) => glossary },
      derivations = triedDerivations.collect { case Success(derivation) => derivation },
      jsonConversionsProviders = triedJsonConversionsProviders.collect { case Success(jsonConversionsProvider) => jsonConversionsProvider }
    )
  })

  private def scanAndLoadClasses[T](entries: List[JarEntry], loader: SpecializedClassLoader[T], classLoader: URLClassLoader, mirror: Mirror): List[Try[T]] = {
    entries.map( entry => entry.getName.dropRight(JarLoaderService.CLASS_FILE_SUFFIX.length).replaceAll("/", ".") )
      .filter( loader.precondition )
      .map( loader.preprocessClassName )
      .map( finalClassName => {
        classLoader.loadClass( finalClassName )
        loader.load(finalClassName, mirror)
      })
  }

}

object JarLoaderService {
  val CLASS_FILE_SUFFIX = ".class"
}

case class JarLoadingResults(jarName: String, glossaries: List[Glossary], derivations: List[Berekening], jsonConversionsProviders: List[JsonConversionsProvider])

/**
  * Implementations of this trait are used by the JarLoaderService to identify and load certain classes and objects from
  * a JAR-file.
  *
  * @tparam T the super type shared by all objects / classes loaded by this loader.
  */
trait SpecializedClassLoader[T] {

  /**
    * A first chance for a loader to skip certain class names. Note that these are the raw names of .class-files within a JAR
    * that is being loaded. You need to be aware of Scala's specific naming policies for things like objects and scripts.
    *
    * @param className the (unprocessed) name of a .class-file which is present in the JAR-file being examined. The '.class'-suffix
    *                  will have been removed before calling this function.
    * @return true if you wish to attempt to load something from this particular class, false i fyou wish to skip it.
    */
  def precondition(className: String): Boolean

  /**
    * After filtering out unwanted entries, the JarLoaderService will call this method to allow the loader to do a little preprocessing
    * on the raw class name. Note that the result of this function will be passed on to the Java classloader to attempt to actually load
    * the class.
    *
    * @param className the (unprocessed) name of a .class-file which was previously selected for loading.
    * @return the potentially altered name of the class to ask the ClassLoader to load.
    */
  def preprocessClassName(className: String): String

  /**
    * After filtering and preprocessing, it is now possible to attempt to load the class or object. This function is tasked with doing
    * just that. When successful, it should return a Success-value, wrapping the loaded value.
    *
    * @param className the preprocessed name of a class to load.
    * @param mirror the current Scala reflection mirror.
    * @return the loaded object or (instance of a) class, wrapped in a Success-value, or Failure if anything went wrong.
    */
  def load(className: String, mirror: Mirror): Try[T]
}

/**
  * Processes JAR-entries looking for objects extending Glossary.
  */
class GlossaryClassLoader extends SpecializedClassLoader[Glossary] {
  // Note: since Glossaries are objects, we should only consider classes ending with a $, as per the Scala compiler's naming convention
  override def precondition(className: String): Boolean = className.endsWith("$")

  // Note: the dropRight(1) ensures the $-sign at the end of the name is removed. The Scala mirror will have no clue what to with the $ and simply requires the name of the object.
  override def preprocessClassName(className: String): String = className.dropRight(1)

  override def load(className: String, mirror: _root_.scala.reflect.runtime.universe.Mirror): Try[Glossary] = Try({
    val glossaryModule: ModuleSymbol = mirror.staticModule(className).asModule

    val modMirror: ModuleMirror = mirror.reflectModule(glossaryModule)

    modMirror.instance.asInstanceOf[Glossary]
  })
}

/**
  * Processes JAR-entries looking for classes extending Berekening, which we will instantiate.
  */
class DerivationClassLoader extends SpecializedClassLoader[Berekening] {
  // Note: since Berekeningen are classes, we should only consider classes NOT ending with a $, because those are objects.
  override def precondition(className: String): Boolean = !className.endsWith("$")

  override def preprocessClassName(className: String): String = className

  override def load(className: String, mirror: _root_.scala.reflect.runtime.universe.Mirror): Try[Berekening] = Try({
    val derivationClass: ClassSymbol = mirror.staticClass(className)

    val classMirror: ClassMirror = mirror.reflectClass(derivationClass)

    val constructor: MethodSymbol = derivationClass.primaryConstructor.asMethod

    val constructorMirror: MethodMirror = classMirror.reflectConstructor(constructor)

    constructorMirror().asInstanceOf[Berekening]
  })
}

/**
  * Processes JAR-entries looking for objects extending JsonConversionMap.
  */
class JsonConversionMapClassLoader extends SpecializedClassLoader[JsonConversionsProvider] {
  // Note: since Glossaries are objects, we should only consider classes ending with a $, as per the Scala compiler's naming convention
  override def precondition(className: String): Boolean = className.endsWith("$")

  // Note: the dropRight(1) ensures the $-sign at the end of the name is removed. The Scala mirror will have no clue what to with the $ and simply requires the name of the object.
  override def preprocessClassName(className: String): String = className.dropRight(1)

  override def load(className: String, mirror: _root_.scala.reflect.runtime.universe.Mirror): Try[JsonConversionsProvider] = Try({
    val jsonConversionsProviderModule: ModuleSymbol = mirror.staticModule(className).asModule

    val modMirror: ModuleMirror = mirror.reflectModule(jsonConversionsProviderModule)

    modMirror.instance.asInstanceOf[JsonConversionsProvider]
  })
}

