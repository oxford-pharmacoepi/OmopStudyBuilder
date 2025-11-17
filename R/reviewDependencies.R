reviewStudyDependencies <- function(dir){


  describeRenv(dir)
  checkRenvPkgs()

  return(invisible())


}
