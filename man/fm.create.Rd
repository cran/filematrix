\name{fm.create}
\alias{fm.create}
\alias{fm.create.from.matrix}
\alias{fm.open}
\alias{fm.load}
\alias{fm.create.from.text.file}
\alias{close,filematrix-method}

\title{
	Functions to create new or open existing file-backed matrices.
}
\description{
 Create a new or open existing \code{\linkS4class{filematrix}} object.
 
 \code{fm.create} creates a new file file-backed matrix. If a file-backed matrix with this name exists, it is overwritten.
 
 \code{fm.create.from.matrix} creates a new file-backed matrix from an existing R matrix.
 
 \code{fm.open} opens an existing file-backed matrix.
 
 \code{fm.load} loads entire existing file-backed matrix into memory.
 
 \code{fm.create.from.text.file} reads a matrix from a text file and saves it into a new file-backed matrix. The rows in the text file become columns in the file-backed matrix.
}
\usage{
fm.create( 
    filenamebase, 
    nrow = 0, 
    ncol = 1,
    type = "double", 
    size = NULL)
    
fm.create.from.matrix( 
    filenamebase, 
    mat, 
    size = NULL)

fm.open(
    filenamebase, 
    readonly = FALSE)

fm.load( filenamebase )

fm.create.from.text.file(
    textfilename, 
    filenamebase, 
    skipRows = 1, 
    skipColumns = 1, 
    sliceSize = 1000, 
    omitCharacters = "NA", 
    delimiter = "\t", 
    rowNamesColumn = 1, 
    type="double", 
    size = NULL)

\S4method{close}{filematrix}(con)

}
\arguments{
 \item{filenamebase}{
  Name without extension for the files storing the file-backed matrix. \cr
  The file \code{<filenamebase>.bmat} stores the matrix values and \code{<filenamebase>.desc.txt} stores the matrix dimensions, data type, and data type size.
 }
 \item{nrow}{
  Number of rows in the matrix. Values over 2^32 are supported.
 }
 \item{ncol}{
  Number of columns in the matrix. Values over 2^32 are supported.
 }
 \item{type}{
  The type of values stored in the matrix. Must be either \code{"double"}, \code{"integer"}, \code{"logical"}, or \code{"raw"}.
 }
 \item{size}{
  Size of each item of the matrix in bytes. \cr
  Default values are 8 for \code{"double"}, 4 for \code{"integer"}, and 1 for \code{"logical"} and \code{"raw"}. \cr
  Do not specify if not sure.
 }
 \item{mat}{
 	Usual R matrix, to be copied to a new a file-backed matrix.
 }
 \item{readonly}{
  If \code{TRUE}, the values in the opened file-backed matrix cannot be changed.
 }
 \item{textfilename}{
  Name of the text file with matrix data, to be copied to a new a file-backed matrix.
 }
 \item{skipRows}{
  Number of rows with column names. The matrix values are expected after first \code{skipRows} rows of the file. Can be zero.
 }
 \item{skipColumns}{
  Number of columns before matrix values begin. Can be zero.
 }
 \item{sliceSize}{
  The text file with matrix is read in chuncks of \code{sliceSize} rows.
 }
 \item{omitCharacters}{
  The text string representing missing values.
 }
 \item{delimiter}{
  The delimiter separating values in the text matrix file.
 }
 \item{rowNamesColumn}{
  The row names are taken from the \code{rowNamesColumn}-th column of the text file.
 }
 \item{con}{
  A file-backed matrix.
 }
}

\details{
  Once created or opened, file-backed matrix object can be accessed as a regular matrix using both vector \code{fm[]} and matrix \code{fm[,]} indexing. The indices can be integers or logical vectors.
}
\value{
	Returns a \code{\link{filematrix-class}} object.
	The object can be closed with \code{close} command.
}
%\references{
%	The package website: \url{http://shabal.in/}
%}
\author{
	Andrey Shabalin \email{ashabalin@vcu.edu}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  For more on the use of file-backed matrices see \code{\linkS4class{filematrix}}.
}
\examples{
filename = tempfile();

fm = fm.create(filenamebase=filename, nrow=10, ncol=10);

fm[1:3,1:3] = 1:9;

print(fm[1:4,1:4])

close(fm);

file.remove(paste0(filename,".bmat"));
file.remove(paste0(filename,".desc.txt"));
}
%\keyword{ file }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
