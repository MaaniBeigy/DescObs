# First let us making some objects
a <- 1
b <- "female"
c <- FALSE
d <- 4:10
e <- 5
f <- 6
g <- 7
df1 <- 8
df2 <- 9
dfcol <- 10
dff1 <- 11
dfff3 <- 12
mydata1 <- 13
mymydata <- 14
dataframe1 <- data.frame(
    gender = c("F", "M", "M", "F"),
    score = c(10, 12, 14, 18)
    )
matrix.a <- matrix(1:10, nrow = 2, ncol = 5)
matrix2 <- matrix(1:100, ncol = 2)
x <- c("a", "b", "c", "d")
y <- c("d", "e", "f")
# Remove objects by rm.versatile
rm_versatile(
    save.objects = list("a", "b", "c"),
    save.patterns = list("df", "data"),
    rm.objects = list("x", "y"),
    rm.patterns = list("matrix")
)
# List objects
ls()

