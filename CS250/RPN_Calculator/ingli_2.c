// RPN Calculator
// Brandon Ingli
// Segments adapted from "The C Programming Language", Kernighan and Ritchie
// 22 March 2018

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define MAX_OP_LEN 50
#define MAX_IPT_LEN 151
#define NUMBER '0'

struct node{
  double value;
  struct node * next;
};

struct node * top = NULL;
unsigned int eqCount = 1;
char ipt[MAX_IPT_LEN];
unsigned int iptIdx = 0, seenOperator = 0;

char getOp(char []);
int pop();
int push(double);
void skipEq();
void onPushError();
void onPopError();
void deleteStack();
double calculate(double, double, char);

int main(){
  unsigned int succ, i = 0;
  double op1, op2;
  char c, op[MAX_OP_LEN], type;

  printf("Enter Your RPN equation(s), one per line:\n");

  ipt[i] = getchar();
  while(ipt[i] != EOF){
    ipt[++i] = getchar();
  }
  ipt[i] = '\0';

  type = getOp(op);
  while(type != '\0'){
    switch(type){
      case NUMBER:
        push(atof(op));
        break;

      case '+':
      case '-':
      case '*':
      case '/':
        if(top == NULL){
          printf("ERROR: NO OPERANDS IN EQUATION %d. SKIPPING EQUATION.\n", eqCount);
  	      skipEq();
  	      break;
        }

        double op2 = top->value;
        succ = pop();
        if(!succ){
          onPopError();
        }

        if(top == NULL){
          printf("ERROR: ONLY ONE OPERAND IN EQUATION %d. SKIPPING EQUATION.\n", eqCount);
  	      skipEq();
  	      break;
        }

        double op1 = top->value;
        succ = pop();
        if(!succ){
          onPopError();
        }

        if(op2 == 0.0 && type == '/'){
          printf("ERROR: DIVIDE BY ZERO IN EQUATION %d: %f / %f . SKIPPING EQUATION.\n", eqCount, op1, op2);
          skipEq();
          break;
        }

        double res = calculate(op1, op2, type);

        succ = push(res);
        if(!succ){
          onPushError();
        }

        seenOperator++;

        break;

      case '\n':
        if(top == NULL || !seenOperator){
  	      printf("ERROR: NO ANSWER TO EQUATION %d.\n", eqCount);
  	      eqCount++;
          deleteStack();
  	      break;
        }

        if(top->next != NULL){
          printf("WARNING: EXTRA OPERAND(S) IN EQUATION %d.\n", eqCount);
        }

        printf("The answer to equation %d is %f\n", eqCount, top->value);
        eqCount++;
        deleteStack();
        seenOperator = 0;
        break;

      default:
        printf("ERROR: UNKNOWN COMMAND %s IN EQUATION %d\n", op, eqCount);
    }

    type = getOp(op);
  }
  deleteStack();
  return 0;
}

char getOp(char s[]){
  unsigned int i;
  char c;

  while((s[0] = c = ipt[iptIdx++]) == ' ' || c == '\t'); //s[0] and c become first non space or tab char

  if (!isdigit(c) && c != '.'){
    return c;
  }

  i = 0;

  if(isdigit(c)){
    while (isdigit(s[++i] = c = ipt[iptIdx++])); //Gets integer component and decimal point
  }

  if(c == '.'){
    while (isdigit(s[++i] = c = ipt[iptIdx++])); //Gets fractional component
  }

  s[i] = '\0';

  ipt[--iptIdx] = c; //Put back an extra char we took from the input

  return NUMBER;

}

int push(double val){
  struct node * newNode = malloc(sizeof(struct node));
  if(newNode == NULL){
    return 0;
  }

  newNode->value = val;
  newNode->next = top;

  top = newNode;

  return 1;
}

void onPushError(){
  printf("ERROR: FAILED PUSH. EXITING.\n");
  deleteStack();
  exit(1);
}

int pop(){
  if(top == NULL){
    return 0;
  }

  struct node * temp = top->next;
  free(top);
  top = temp;

  return 1;
}

void onPopError(){
  printf("ERROR: TRIED POP FROM EMPTY STACK. EXITING.\n");
  exit(2);
}

void deleteStack(){
  while(top != NULL){
    pop();
  }
}

void skipEq(){
  char c;

  deleteStack();
  eqCount++;
  seenOperator = 0;
  while((c = ipt[iptIdx++]) != '\n' && c != '\0');
}

double calculate(double op1, double op2, char operator){
  switch(operator){
    case '+':
      return op1 + op2;
    case '-':
      return op1 - op2;
    case '*':
      return op1 * op2;
    case '/':
      if(op2 != 0.0){
        return op1 / op2;
      }
      else{
        printf("ERROR: DIVIDE BY ZERO IN EQUATION %d. EXITING\n", eqCount, op1, op2);
        deleteStack();
        exit(3);
      }
    default:
      printf("ERROR: UNKNOWN OPERATOR '%c' IN EQUATION %d. EXITING\n", operator, eqCount);
      deleteStack();
      exit(4);
  }
}
