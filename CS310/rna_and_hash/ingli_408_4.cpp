/**
 * Program to count collisions in a hash table of the Unix dictionary
 * @author Brandon Ingli
 * @version 5 April 2019
 */

#include <iostream>
#include <vector>

using namespace std;

/**
 * The hash function for the Unix dictionary hash table.
 */
size_t hash_fn(const string& key, size_t table_size);

int main()
{
  const size_t TABLE_SIZE {102301};
  vector<bool> word_seen (TABLE_SIZE, false);
  size_t collisions_detected {0};
  string word;

  while (getline(cin, word))
  {
    size_t hash_val = hash_fn(word, TABLE_SIZE);
    if (word_seen.at(hash_val))
    {
      collisions_detected++;
    }
    else
    {
      word_seen.at(hash_val) = true;
    }
    
  }

  cout << collisions_detected << endl;

  return 0;
}

 size_t hash_fn(const string& key, size_t table_size)
{
  size_t hash_val {0};
  for (auto character : key)
  {
    hash_val = 37 * hash_val + static_cast<size_t>(character);
  }
  return hash_val % table_size;
}

