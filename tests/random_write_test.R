n = 20;

library(filematrix)
fm = fm.create(filenamebase = tempfile(), n, n)
mat = matrix(0, n, n);


rownames(fm) = as.character(1:nrow(fm));
colnames(fm) = as.character(1:ncol(fm));

### Fully indexed access

k = 10;
for( test in 1:20 ) {

	cat('Fully indexed access, test', test,'\n');
	rowset = sample.int(n, size = k);
	colset = sample.int(n, size = k);
	
	value = matrix( runif(length(rowset)*length(colset)), length(rowset), length(colset));
	
	stopifnot( all( fm[rowset, colset] == mat[rowset, colset] ) );

	fm[rowset, colset] = value;
	mat[rowset, colset] = value;
}
stopifnot( all( as.matrix(fm) == mat ) );


### All columns access

k = 10;
for( test in 1:20 ) {
	
	cat('All columns access, test', test,'\n');
	colset = sample.int(n, size = k);
	
	value = matrix( runif(length(rowset)*n), length(rowset), n);
	
	stopifnot( all( fm[rowset, ] == mat[rowset, ] ) );

	fm[rowset, ] = value;
	mat[rowset, ] = value;
}
stopifnot( all( as.matrix(fm) == mat ) );


### All rows access

k = 10;
for( test in 1:20 ) {
	
	cat('All rows access, test', test,'\n');
	colset = sample.int(n, size = k);
	
	value = matrix( runif(n*length(colset)), n, length(colset));
	
	stopifnot( all( fm[, colset] == mat[, colset] ) );

	fm[, colset] = value;
	mat[, colset] = value;
}
stopifnot( all( as.matrix(fm) == mat ) );


### Vector access

k = 10;
for( test in 1:20 ) {
	
	cat('Vector access, test', test,'\n');
	set = sample.int(n^2, size = k^2);
	
	value = runif(k^2);
	
	stopifnot( all( fm[set] == mat[set] ) );
	
	fm[set] = value;
	mat[set] = value;
}
stopifnot( all( as.matrix(fm) == mat ) );

closeAndDeleteFiles(fm);
