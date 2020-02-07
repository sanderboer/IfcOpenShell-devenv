#include <iostream>

int count()
{
static int c=5;
c++;
return c;
}

int main(){
for (int i=0; i<100; i++)
{

  std::cout<< "count: " << count() <<std::endl;
  std::cout<< "c: " << c <<std::endl;

}

}
