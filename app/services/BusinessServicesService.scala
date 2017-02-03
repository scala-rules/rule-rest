package services

import javax.inject.{Inject, Singleton}

import org.scalarules.service.dsl.BusinessService
import play.api.Configuration

@Singleton
class BusinessServicesService @Inject() (configuration: Configuration, jarLoaderService: JarLoaderService) {

  val businessServices: List[(String, BusinessService)] = jarLoaderService.jars.flatMap {
    case (jarName: String, jarLoadingResults: JarLoadingResults) => {
      jarLoadingResults.businessServices.map( businessService => (businessService.getClass.getSimpleName, businessService))
    }
  }.toList

  val businessServiceNames: List[String] = businessServices.map{ case (name, _) => name }

}
