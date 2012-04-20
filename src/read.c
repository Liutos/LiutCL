#include "types.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct StringPart {
    char *part;
    int length;
    struct StringPart *next;
};

char *read_line(FILE *fp, int *length) /* This function preserve the last '\n' character in the string returned. */
{
    char c, *line;
    int i, len;

    c = fgetc(fp);
    line = NULL;
    i = len = 0;
    while (c != EOF) {
	if (i >= len) {
	    len += 80;
	    line = realloc(line, len * sizeof(char));
	}
	line[i++] = c;
	if ('\n' == c) {
	    line = realloc(line, (i + 1) * sizeof(char));
	    line[i] = '\0';
	    break;
	}
	c = fgetc(fp);
    }
    if (line != NULL && EOF == c)
	line[i] = '\0';
    *length = i;

    return line;
}

int is_balance(char *expression)
{
    int balance, i;

    balance = 0;
    i = -1;
    do {
	i++;
	if ('(' == expression[i] && i > 0 &&
	    '\\' == expression[i - 1]) balance++;
	if (')' == expression[i] && i > 0 &&
	    '\\' == expression[i - 1]) balance--;
    } while (expression[i] != '\0');

    return balance;
}

struct StringPart *read_expression_core(FILE *fp)
{
    char *part;
    int balance, length;
    struct StringPart *cur, *head, *pre;

    balance = 0;
    head = malloc(sizeof(struct StringPart));
    pre = head;
    do {
	part = read_line(fp, &length);
	if (part != NULL) {
	    cur = malloc(sizeof(struct StringPart));
	    cur->part = part;
	    cur->length = length;
	    cur->next = NULL;
	    pre->next = cur;
	    pre = cur;
	    balance += is_balance(part);
	}
    } while (balance != 0);

    return head;
}

char *concatenate_part(struct StringPart *part_list)
{
    char *string;
    int i, total_length;
    struct StringPart *part_node;

    part_node = part_list->next;

    total_length = 0;
    while (part_node != NULL) {
	total_length += part_node->length;
	part_node = part_node->next;
    }

    string = malloc((total_length + 1) * sizeof(char));
    part_node = part_list->next;
    i = 0;
    while (part_node != NULL) {
	strncpy(string + i, part_node->part, part_node->length);
	i += part_node->length;
	part_node = part_node->next;
    }
    string[total_length] = '\0';

    return string;
}

char *read_expression(FILE *fp)
{
    return concatenate_part(read_expression_core(fp));
}
