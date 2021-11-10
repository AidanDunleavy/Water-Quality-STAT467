class Cluster {     // The class
  public:           // Access specifier
    Cluster(NumericVector neighborhood) {     // Constructor
      NumericVector this->neighborhood = neighborhood;
      
    }
    
};

int main() {
  Cluster myObj;    // Create an object of MyClass (this will call the constructor)
  return 0;
}