#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int my_knn_cpp(NumericMatrix X, NumericVector X0, NumericVector y) {
  
  int nrows=  X.nrow();
  int ncols=  X.ncol();
  
  double closest_distance = 99999999;
  int closest_output=-1;
  int closest_neighbor=-1;
  
  double diff=0;
  
  for(int i=0; i<nrows;i++){
    double distance =0;
    for(int j=0; j<ncols; j++){
      diff=X(i,j)-X0[j];
      distance+=diff*diff;
      
    }
    
    if(distance<closest_distance){
      closest_distance=distance;
      closest_output=y[i];
      closest_neighbor=i;
    }
    
  }
  
  return closest_output;
}

