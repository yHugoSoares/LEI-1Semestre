#ifndef COMMAND_PARSER_H
#define COMMAND_PARSER_H

#include "deque.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct cmd
{
    char *command;
    int *args;
    int nargs;
} Cmd;

void processCommand(Deque *deque, Cmd *cmd);
Cmd* parseLine(char* line);

#endif