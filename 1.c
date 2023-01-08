void main(){
  int x, y;
  x = 1;
  cai_assume(y >= -4 && y <= 4);
  if (y){
    y = 2*x + 1;
  }else{
    y = 3*x + 2;
  }
  if(y > 3){
    x = 1/x;
  }
}

