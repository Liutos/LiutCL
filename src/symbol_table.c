/*
 * symbol_table.c
 *
 * Implements the symbol table as an AVL tree and its operators.
 *
 * Copyright (C) 2012-10-17 liutos mat.liutos@gmail.com
 */
#include "symtbl.h"
#include "types.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

extern Symbol make_symbol(char *);

SymbolTable symbol_table = NULL;

char *get_node_key(StrSymMapNode node)
{
    return node->symbol->symbol_name;
}

/* Query for a symbol by its name. */
Symbol get_symbol(char *symbol_name)
{
    SymbolTable tbl;

    tbl = symbol_table;
    while (tbl != NULL) {
        if (0 == strcmp(get_node_key(tbl), symbol_name))
            return tbl->symbol;
        if (strcmp(symbol_name, get_node_key(tbl)) < 0)
            tbl = tbl->left;
        else
            tbl = tbl->right;
    }

    return NULL;
}

StrSymMapNode make_node(Symbol symbol)
{
    StrSymMapNode node;

    node = malloc(sizeof(struct StrSymMapNode));
    node->symbol = symbol;
    node->left = node->parent = node->right = NULL;
    node->height = node->bf = 0;

    return node;
}

/* The function for comparing two objects of type `StrSymMapNode'. */
int node_compare(StrSymMapNode node1, StrSymMapNode node2)
{
    return strcmp(get_node_key(node1), get_node_key(node2));
}

int get_max(int a, int b)
{
    return a > b? a: b;
}

int get_tree_height(SymbolTable sym_tbl)
{
    return NULL == sym_tbl? -1: sym_tbl->height;
}

int compute_tree_height(SymbolTable table)
{
    return get_max(get_tree_height(table->left),
                   get_tree_height(table->right)) + 1;
}

int compute_tree_bf(SymbolTable table)
{
    return get_tree_height(table->left) - get_tree_height(table->right);
}


SymbolTable left_rotate(SymbolTable tree)
{
    SymbolTable tmp;

    tmp = tree->right;
    tree->right = tmp->left;
    tmp->left = tree;
    tmp->bf = 0;
    tree->bf = 0;
    tree->height = compute_tree_height(tree); /* Update the tree->height first. */
    tmp->height = compute_tree_height(tmp);   /* Update the height of the root of
                                                 the new tree. */

    return tmp;
}

SymbolTable right_rotate(SymbolTable tree)
{
    SymbolTable tmp;

    tmp = tree->left;
    tree->left = tmp->right;
    tmp->right = tree;
    tmp->bf = 0;
    tree->bf = 0;
    tree->height = compute_tree_height(tree); /* The same as the left_rotate. */
    tmp->height = compute_tree_height(tmp);

    return tmp;
}

SymbolTable put_symbol_core(StrSymMapNode node, SymbolTable sym_tbl)
{
    if (NULL == sym_tbl)
        return node;
    if (0 == node_compare(node, sym_tbl)) {
        printf("It's impossible.\n");
        exit(1);
    }
    if (node_compare(node, sym_tbl) > 0) {
        sym_tbl->right = put_symbol_core(node, sym_tbl->right);
        sym_tbl->height = compute_tree_height(sym_tbl);
        sym_tbl->bf = compute_tree_bf(sym_tbl);
        /* If both sym_tbl->bf and sym_tbl->right->bf are negative, it's the case of RR. */
        if (-2 == sym_tbl->bf && -1 == sym_tbl->right->bf)
            sym_tbl = left_rotate(sym_tbl);
        /* If sym_tbl->bf is negative and sym_tbl->right->bf is positive, it's the case of RL. */
        else if (-2 == sym_tbl->bf && 1 == sym_tbl->right->bf) {
            sym_tbl->right = right_rotate(sym_tbl->right);
            sym_tbl = left_rotate(sym_tbl);
        }
    } else {
        sym_tbl->left = put_symbol_core(node, sym_tbl->left);
        sym_tbl->height = compute_tree_height(sym_tbl);
        sym_tbl->bf = compute_tree_bf(sym_tbl);
        /* If both sym_tbl->bf and sym_tbl->left->bf are positive, it's the case of LL. */
        if (2 == sym_tbl->bf && 1 == sym_tbl->left->bf)
            sym_tbl = right_rotate(sym_tbl);
        /* If sym_tbl->bf is positive and sym_tbl->left->bf is negative, it's the case of LR. */
        else if (2 == sym_tbl->bf && -1 == sym_tbl->left->bf) {
            sym_tbl->left = left_rotate(sym_tbl->left);
            sym_tbl = right_rotate(sym_tbl);
        }
    }

    return sym_tbl;
}

/* The interface function for using. */
void put_symbol(Symbol symbol)
{
    StrSymMapNode node;

    node = make_node(symbol);
    symbol_table = put_symbol_core(node, symbol_table);
}

/* Ensure the symbol with name `symbol_name' exists uniquely. */
Symbol ensure_symbol_exists(char *symbol_name)
{
    Symbol symbol;

    symbol = get_symbol(symbol_name);
    if (symbol != NULL)
	return symbol;
    else {
	symbol = make_symbol(symbol_name);
	put_symbol(symbol);

	return symbol;
    }
}
