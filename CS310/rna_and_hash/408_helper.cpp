#include <iostream>
using namespace std;

int hashfn(int);

int main(){
  int values[] = {4371, 1323, 6173, 4199, 9679, 1989};
  for (auto value : values)
  {
    cout << value << " -> " << hashfn(value) << endl;
  }

  return 0;
}

int hashfn(int val){
  return 16 - (val % 17);
}
