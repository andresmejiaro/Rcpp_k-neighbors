
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}

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


// [[Rcpp::export]]
NumericVector minmax_data(NumericVector x) {
  
  double minx=min(x);
  double maxx=max(x);
  
  NumericVector xs=x-minx;
  
  xs=xs/(maxx-minx);
  
  return xs;

}


// [[Rcpp::export]]
NumericVector stand_data(NumericVector x) {
  
  double meanx=mean(x);
  double sdx=sd(x);
  
  NumericVector xs=x-meanx;
  
  xs=xs/sdx;
  
  return xs;
  
}

// [[Rcpp::export]]
double Mink_dist(NumericVector x,NumericVector y,double p) {
  
  double cumulant=0;
  double expre =0;
  
  if(p<=0){
    for(int i=0; i<x.length();i++){
      expre=fabs(x[i]-y[i]);
      if(expre>cumulant){
        cumulant=expre;
      }
    }
    
    return cumulant;
    
  }
  
  
  
  for(int i=0; i<x.length();i++){
    expre=pow(fabs(x[i]-y[i]),p);
    cumulant+=expre;
  }
  
  double k=1/p;
  
  cumulant=pow(cumulant,k);
  
  return cumulant;

}


int knn_completa(NumericMatrix X, NumericVector X0, NumericVector y, int modo) {
  
  
  if(modo==0){
    
  }
  
  
  
  int nrows=  X.nrow();
  int ncols=  X.ncol();
  
  double closest_distance = 99999999;
  int closest_output=-1;
  int closest_neighbor=-1;
  
  double distance=0;

  for(int i=0; i<nrows;i++){
    distance=Mink_dist(X(i,_),X0,2);
    
    if(distance<closest_distance){
      closest_distance=distance;
      closest_output=y[i];
      closest_neighbor=i;
    }
    
  }
  
  return closest_output;
}


