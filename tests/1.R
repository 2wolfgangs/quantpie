test.StandardisedMomentum <- function() {
    
  
  checkEquals(2, testfunction(2,0))
  checkEquals(-20, testfunction(-20,0))
  checkTrue(6 == testfunction(2,3))
}
