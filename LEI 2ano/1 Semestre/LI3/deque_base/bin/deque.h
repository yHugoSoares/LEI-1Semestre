#ifndef _DEQUE_H_
#define _DEQUE_H_

#include <stdbool.h>

/* Base node struct, with pointers to the previous and next nodes */
typedef struct node {
    void* data;
    struct node* prev;
    struct node* next;
} Node;

/* Double-ended queue struct (https://en.wikipedia.org/wiki/Double-ended_queue) */
typedef struct deque {
    int size;
    Node* first;
    Node* last;
    bool reversed;
} Deque;

/* Creates an empty deque */
Deque* create();

/* Adds an element to the end of the deque */
void push(Deque* deque, void* data);

/* Adds an element to the begin of the deque */
void pushFront(Deque* deque, void* data);

/* Removes the first element in the deque and returns it; returns null if empty */
void* pop(Deque* deque);

/* Removes the last element in the deque and returns it; returns null if empty */
void* popFront(Deque* deque);

/* Returns the number of elements in the deque */
int size(Deque* deque);

/* Returns true if the deque is empty */
bool isEmpty(Deque* deque);

/* Reverses the traversal order of the deque */
void reverse(Deque* deque);

/* Prints the elements in the deque using 'printFunc' */
void printDeque(Deque* deque, void(*printFunc)(void*));

/* Frees all the memory allocated by the deque. 
Note that this only frees data allocated by the deque itself, and not by the data it contains. 
*/
void destroy(Deque* deque);

#endif
