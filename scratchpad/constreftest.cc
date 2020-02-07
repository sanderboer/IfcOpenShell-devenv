#include<iostream>

static int getnum(int val)
{
  return val;
}

void printer(const int& val )
{
  std::cout << val << std::endl;
}



int main(){
printer( getnum(16) );

}
