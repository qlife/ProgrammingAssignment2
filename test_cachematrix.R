source('cachematrix.R')

testCacheMatrix <- function() {
        x <- matrix(runif(1000000), ncol=1000)
        print("Compute the basic answer by solve()")
        inv_x <- solve(x)

        loop_eval <- function() {
                for(i in 1:10) {
                        z <- solve(x)
                        if(!all.equal(inv_x, z)) {
                                print("Computed inv different to inv_x")
                                return(FALSE)
                        }
                }
        }


        cm <- makeCacheMatrix()
        cm$set(x)

        loop_cache_eval <- function() {
                for(i in 1:10) {
                        z2 <- cacheSolve(cm)
                        if(!all.equal(z2, inv_x)) {
                                print("Computed inv different to inv_x")
                                return(FALSE)
                        }
                }
        }

        print("Compute inverse of a 1000x1000 random matrix")
        print("No cache version")

        ptm <- proc.time()
        loop_eval()
        print(proc.time() - ptm)

        print("makeCacheVersion")

        ptm <- proc.time()
        loop_cache_eval()
        print(proc.time() - ptm)
}

testReplace <- function () {
        m <- matrix(runif(10000), ncol=100)
        cm <- makeCacheMatrix(m)
        inv_1 <- cacheSolve(cm)
        inv_2 <- cacheSolve(cm)
        if(!all.equal(inv_1,inv_2)) {
                print("Cache changed unexpectedly.")
        }
        m <- m + matrix(runif(10000), ncol=100)
        cm$set(m)
        inv_3 <- cacheSolve(cm)

        if(TRUE == all.equal(inv_1, inv_3)) {
                print("Cache didn't recaculate.")
        }

}
