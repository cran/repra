#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// Function to perform convolution. See "?convolution" in R

// [[Rcpp::export]]
DataFrame convolution_table(NumericVector capacity, NumericVector efor, double threshold) {
  // Initialize variables
  int n = 0;
  double *cap = NULL, *prob = NULL;
  
  for (int i = 0; i < capacity.length(); ++i) {
    double *old_cap = cap;
    double *old_prob = prob;
    
    if (i == 0) {
      // Create table for first generator
      cap = new double[2];
      prob = new double[2];
      n = 2;
      
      cap[0] = capacity[0];
      cap[1] = 0.0;
      prob[0] = 1.0 - efor[0];
      prob[1] = efor[0];
    } else {
      // Convolute i-th generator
      int x1 = 0, x2 = 0, old_n = n, maxN = 2 * n;
      double pushCap, pushProb;
      
      cap = new double[maxN];
      prob = new double[maxN];
      n = 0;

      while (x2 < old_n) {
        if (x1 == old_n) {
          pushCap = old_cap[x2];
          pushProb = old_prob[x2] * efor[i];
          ++x2;
        } else {
          double cap1 = old_cap[x1] + capacity[i];
          double cap2 = old_cap[x2];
          
          if (cap1 > cap2) {
            // Cap1 is larger
            pushCap = cap1;
            pushProb = old_prob[x1] * (1.0 - efor[i]);
            ++x1;
          } else if (cap1 < cap2) {
            // Cap2 is larger
            pushCap = cap2;
            pushProb = old_prob[x2] * efor[i];
            ++x2;
          } else {
            // Tie in cap1 and cap2
            pushCap = cap1;
            pushProb = old_prob[x1] * (1.0 - efor[i]) + old_prob[x2] * efor[i];
            ++x1;
            ++x2;
          }
        }
        
        cap[n] = pushCap;
        prob[n] = pushProb;
        ++n;
      }
    }
    
    // Delete temporary vectors
    delete old_cap;
    delete old_prob;
  }
  
  // Create vectors for data output
  NumericVector outCap(0), outProb(0), outLolp(0), outBaseEue(0);
  double lolp = 1.0;
  for (int i = 0; (i < n) & (lolp >= threshold); ++i) {
    outCap.push_back(cap[i]);
    outProb.push_back(prob[i]);
    outLolp.push_back(lolp);
    
    lolp -= prob[i];
  }
  
  // Calculate fixed term in EUE
  for (int i = 0; i < outCap.size(); ++i) {
    double eue = 0.0;
    for (int j = i + 1; j < outCap.size(); ++j)
      eue += (outCap[i] - outCap[j]) * outProb[j];
    outBaseEue.push_back(eue);
  }
  
  // Delete vectors
  delete cap;
  delete prob;
  
  // Create output as data frame
  return DataFrame::create(Named("Capacity")  = outCap,
                           Named("Capacity2") = outCap,
                           Named("Prob")      = outProb,
                           Named("LOLP")      = outLolp,
                           Named("BaseEUE")   = outBaseEue);
}
