# Assignment for 8 April 2019

Last Modified: 7 April 2019

This assignment is due to be completed and submitted by 1 pm Monday, 8 April.

1. Draw the memo table and memok helper table for the RNA substructure problem using the RNA sequence CAGUCAUAGCAAUGCU, and indicate which letters match in the secondary structure.
2. Given the input 4371, 1323, 6173, 4199, 9679, 1989 for an initially empty hash table, and a hash function h(x) = 6 – (x mod 7), draw the resulting hash table. State and explain any assumptions you make, and explain your resulting table.
3. Rehash the hash table of problem 2, and show the hash table that results from the rehashing. Explain your process and results.
4. Every standard Unix system has a file /usr/share/dict/words which is a newline-delimited utf-8 file with a fair number of common words. The command $ wc -w /usr/share/dict/words will tell you how many words are in the file (the wc command stands for wordcount). Here is a hash function:  
```c++
    size_t hashx(const string& key, size_t table_size)
    {
      size_t hash_val {0};
      for (auto character : key)
      {
        hash_val = 37 * hash_val + static_cast<unsigned char>(character);
      }
      return hash_val % table_size;
    }
```
Implement this algorithm using a table size that approximates a load factor of 1 when the function is used to hash the contents of /usr/share/dict/words as the keys to be hashed. Implement this function in a program that reads words from standard input. Here is a program you should use as the template for your program to read one-word lines from standard input. Run your program with the command line $ ./program < /usr/share/dict/words Count the number of collisions the algorithm produces when the words are hashed. Note that you do not have to implement a hash table, just count how many collisions there would be if you did implement a hash table.  

5. Explain and justify your results. Be sure to explain what your table size is, and why you chose that value.

To aid you in making a reasonable drawing of a hash table for problems 2 and 3, here is a LaTeX file that shows how to use tikz to draw a picture of a hash table.

By the due date, use the homework submission page to submit the .cpp program that implements the hash function for processing the words, the .tex source file, and the finished .pdf writeup. 