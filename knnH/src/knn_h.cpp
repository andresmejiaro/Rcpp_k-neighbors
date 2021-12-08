#include <Rcpp.h>
using namespace Rcpp;

#include <stdio.h>

using namespace Rcpp;
using namespace std;


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
NumericMatrix stand_data_Matrix(NumericMatrix X) {
  NumericMatrix X_outmm=clone(X);
  for(int i=0;i<X_outmm.ncol();i++) {
    X_outmm(_,i) = stand_data(X_outmm(_,i));
  }
  return(X_outmm);
}

// [[Rcpp::export]]
NumericMatrix minmax_data_Matrix(NumericMatrix X) {
  NumericMatrix X_outm=clone(X);
  for(int i=0;i<X_outm.ncol();i++){
    X_outm(_,i)=minmax_data(X_outm(_,i));
  }
  return X_outm;
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

// [[Rcpp::export]]
int knn_completa(NumericMatrix X, NumericVector X0, NumericVector y, int modo, int mk) {
  
  NumericMatrix X_out=clone(X);
  NumericVector X0_out=clone(X0);
  
  if(modo==0 || modo==1){
    if(modo==0){
      X_out=stand_data_Matrix(X_out);
      for(int i=0;i<X.ncol();i++){
        double meanx=mean(X(_,i));
        double sdx=sd(X(_,i));
        X0_out[i]=(X0[i]-meanx)/sdx;
      }
      
    }
    else{
      
      X_out= minmax_data_Matrix(X_out);
      for(int i=0;i<X.ncol();i++) {
        double minx=min(X(_,i));
        double maxx=max(X(_,i));
        X0_out[i]=(X0[i]-minx)/(maxx-minx);
      }
    }
  }
  
  else{
    printf("Fourth argument must be 0 or 1");
    return(NULL);
  }
  
  int nrows=  X_out.nrow();
  
  double closest_distance = 99999999;
  int closest_output=-1;
  int closest_neighbor=-1;
  
  double distance=0;
  
  for(int i=0; i<nrows;i++){
    distance=Mink_dist(X_out(i,_),X0_out,mk);
    
    if(distance<closest_distance){
      closest_distance=distance;
      closest_output=y[i];
      closest_neighbor=i;
    }
    
  }
  
  return closest_output;
}
