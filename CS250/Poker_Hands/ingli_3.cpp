/* Brandon Ingli
CS 250 Assignment 3
Spring 2018 */

#include <iostream>
#include <string>
#include <cstdlib>
#include "ingli_card.h"
#include "ingli_deck.h"
using namespace std;

string rankToStr(int);
int numPairs(Card[], int);
int numOfKind(Card[], int);
bool hasFlush(Card[], int);
bool hasStraight(Card[], int);

const int NUM_HANDS = 2;
const int NUM_CARDS_PER_HAND = 5;


int main(){
  char input;
  DeckOfCards deck(SHUFFLE);

  do{
    Card hands[NUM_HANDS][NUM_CARDS_PER_HAND];
    int ranks[NUM_HANDS];

    for(int i = 0; i < NUM_HANDS; i++){
      for(int j = 0; j < NUM_CARDS_PER_HAND; j++){
        hands[i][j] = deck.dealCard();
      }
    }

    for(int i = 0; i < NUM_HANDS; i++){
      ranks[i] = numPairs(hands[i], NUM_CARDS_PER_HAND);
      if(ranks[i] > 2){
        ranks[i] = 2;
      }
      int maxOfKind = numOfKind(hands[i], NUM_CARDS_PER_HAND);
      if(maxOfKind >= 3){
        ranks[i] = 3;
      }
      if(maxOfKind >= 4){
        ranks[i] = 4;
      }
      if(hasFlush(hands[i], NUM_CARDS_PER_HAND)){
        ranks[i] = 5;
      }
      if(hasStraight(hands[i], NUM_CARDS_PER_HAND)){
        ranks[i] = 6;
      }
    }

    int topHand = 0;
    int topHandRank = ranks[0];
    for(int i = 1; i < NUM_HANDS; i++){
      if(ranks[i] > topHandRank){
        topHand = i;
        topHandRank = ranks[i];
      }
    }

    for(int i = 0; i < NUM_HANDS; i++){
      cout<< "Hand " << i+1 << ": ";
      for(Card c : hands[i]){
        cout << c;
      }
      cout << endl << "\tThis hand has " << rankToStr(ranks[i]) << "." << endl << endl;
    }

    cout << "The best hand was Hand " << topHand+1 << " because it had " << rankToStr(ranks[topHand]) << "." << endl;

    cout << "Another round? [y/n]: ";
    cin >> input;
    cout << endl << endl << endl;
  }while(input == 'y' || input == 'Y');

  return 0;
}

string rankToStr(int rank){
  switch(rank){
    case 1: return "A Single Pair";
    case 2: return "Two or More Pairs";
    case 3: return "Three of a Kind";
    case 4: return "Four or More of a Kind";
    case 5: return "A Flush";
    case 6: return "A Straight";
    default: return "No Special Combos";
  }
}


int numPairs(Card hand[], int len){
  int faceCounter = 1;
  int suitCounter = 1;
  for(int i = 0; i < len; i++){
    for(int j = 0; j < len; j++){
      if(i != j && hand[i].getFace() == hand[j].getFace()){
        faceCounter++;
      }
      if(i != j && hand[i].getSuit() == hand[j].getSuit()){
        suitCounter++;
      }
    }
  }

  return (faceCounter+suitCounter-1)/2;
}

int numOfKind(Card hand[], int len){
  int maxOfKind = 1;
  for(int i = 0; i < len; i++){
    int faceCounter = 0;
    int suitCounter = 0;

    for(int j = 0; j < len; j++){
      if(hand[i].getFace() == hand[j].getFace()){
        faceCounter++;
      }
      if(hand[i].getSuit() == hand[j].getSuit()){
        suitCounter++;
      }
    }
    if(faceCounter > maxOfKind){
      maxOfKind = faceCounter;
    }
    if(suitCounter > maxOfKind){
      maxOfKind = suitCounter;
    }
  }

  return maxOfKind;
}

bool hasFlush(Card hand[], int len){
  int suitCounts[] = {0, 0, 0, 0};
  for(int i = 0; i < len; i++){
    suitCounts[(int) hand[i].getSuit()]++;
  }

  for(int count : suitCounts){
    if(count >= 5){
      return true;
    }
  }
  return false;
}

void sortHand(Card hand[], int len){
  for(int i = 0; i < len-1; i++){
    int minIdx = i;
    for(int j = i+1; j<len; j++){
      if(hand[j] < hand[minIdx]){
        minIdx = j;
      }
    }
    if(minIdx != i){
      Card temp = hand[i];
      hand[i] = hand[minIdx];
      hand[minIdx] = temp;
    }
  }
}

bool hasStraight(Card hand[], int len){
  sortHand(hand, len);

  int maxStreak = 1;
  int currStreak = 1;
  for(int i = 0; i < len-1; i++){
    if((int) hand[i+1].getFace() == (int) hand[i].getFace() + 1){
      currStreak++;
    }
    else{
      if(currStreak > maxStreak){
        maxStreak = currStreak;
      }
      currStreak = 1;
    }
  }

  return maxStreak >= 5;
}
