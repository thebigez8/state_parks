#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector three_opt(IntegerVector tour, NumericMatrix distances) {
  bool found = false;
  IntegerVector out = clone(tour);
  int n = distances.ncol();
  for(int i = 0; i < n && !found; i++)
  {
    for(int j = i + 2; j < n && !found; j++)
    {
      for(int k = j + 2; k < n - (i == 0) && !found; k++)
      {
        int i2 = i + 1, j2 = j + 1, k2 = k + 1;
        k2 = k2 % n;
        int x = out[i], x2 = out[i2], y = out[j], y2 = out[j2], z = out[k], z2 = out[k2];
        NumericVector dists(8);
        dists[0] = distances(x, x2) + distances(y, y2) + distances(z, z2);
        dists[1] = distances(x, x2) + distances(y, z) + distances(y2, z2);
        //  dists[ ] = distances(x, x2) + distances(y, z2) + distances(y2, z);

        dists[2] = distances(x, y) + distances(x2, y2) + distances(z, z2);
        dists[3] = distances(x, y) + distances(x2, z) + distances(y2, z2);
        //  dists[ ] = distances(x, y) + distances(x2, z2) + distances(y2, z);

        //  dists[ ] = distances(x, y2) + distances(x2, y) + distances(z, z2);
        dists[4] = distances(x, y2) + distances(x2, z) + distances(y, z2);
        dists[5] = distances(x, y2) + distances(x2, z2) + distances(y, z);

        //  dists[ ] = distances(x, z) + distances(x2, y) + distances(y2, z2);
        dists[6] = distances(x, z) + distances(x2, y2) + distances(y, z2);
        dists[7] = distances(x, z) + distances(x2, z2) + distances(y, y2);

        //  dists[ ] = distances(x, z2) + distances(x2, y) + distances(y2, z);
        //  dists[ ] = distances(x, z2) + distances(x2, y2) + distances(y, z);
        //  dists[ ] = distances(x, z2) + distances(x2, z) + distances(y, y2);

        int minidx = which_min(dists);
        found = (minidx != 0);
        if(minidx == 2 || minidx == 3 || minidx == 4 || minidx == 6)
        {
          // swap x2 and y
          for(int m = 0; i2 + m <= j; m++) out[i2 + m] = tour[j - m];
        }
        if(minidx == 1 || minidx == 3 || minidx == 4 || minidx == 5)
        {
          // swap y2 and z
          for(int m = 0; j2 + m <= k; m++) out[j2 + m] = tour[k - m];
        }
        if(minidx == 4 || minidx == 5 || minidx == 6 || minidx == 7)
        {
          // swap z2 and x
          for(int m = 0; (k2 + m) % n <= i; m++) out[(k2 + m) % n] = tour[(i + n - m) % n];
        }
      }
    }
  }
  if(found) return three_opt(out, distances);
  return out;
}
