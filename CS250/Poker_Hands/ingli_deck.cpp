/* Brandon Ingli
CS 250 Assignment 3
Spring 2018 */

#include "ingli_deck.h"
#include "ingli_card.h"
#include <string>
#include <cstdlib>
#include <iostream>
using namespace std;

ostream& operator<<(ostream& os, const DeckOfCards& rhs){
  for(int i = rhs.curr_card; i < 52; i++){
    os << rhs.deck[i];
  }
  os << endl;
  return os;
}

DeckOfCards::DeckOfCards(bool shuffle){
  srand(time(NULL));
  shuffleOnReload = shuffle;
  reloadDeck();
}

DeckOfCards::DeckOfCards(){
  srand(time(NULL));
  shuffleOnReload = SHUFFLE;
  reloadDeck();
}

void DeckOfCards::reloadDeck(){
  curr_card = 0;

  for(int i = 0; i < 4; i++){
    for(int j = 2; j < 15; j++){
      deck[curr_card] = Card((Card::Face) j, (Card::Suit) i);
      curr_card++;
    }
  }

  curr_card = 0;
  if(shuffleOnReload){
    shuffle();
  }
}

bool DeckOfCards::isEmpty(){
  return curr_card >= 52;
}

Card DeckOfCards::dealCard(){
  if(this->isEmpty()){
    cerr << "[MESSAGE] Deck is empty. Reloading then dealing card." << endl;
    this->reloadDeck();
    return this->dealCard();
  }

  curr_card = curr_card + 1;
  return deck[curr_card - 1];
}

void DeckOfCards::shuffle(){
  for(int i = 0; i < 52; i++){
    int j = rand() % 52;
    DeckOfCards::swap(i, j);
  }
}

void DeckOfCards::swap(int i, int j){
  Card temp = deck[j];
  deck[j] = deck[i];
  deck[i] = temp;
}
