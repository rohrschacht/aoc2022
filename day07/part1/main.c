#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct LinkedListElement {
    void *element;
    struct LinkedListElement *next;
    struct LinkedListElement *previous;
} LinkedListElement;

typedef struct LinkedList {
    LinkedListElement *head;
    LinkedListElement *tail;
} LinkedList;

void add_element(LinkedList *list, void *element) {
    LinkedListElement *linkedListElement = calloc(1, sizeof(LinkedListElement));
    linkedListElement->element = element;
    linkedListElement->next = NULL;
    linkedListElement->previous = list->tail;
    if (list->tail) {
        list->tail->next = linkedListElement;
    }
    list->tail = linkedListElement;
    if (list->head == NULL) {
        list->head = linkedListElement;
    }
}

LinkedList *new_list() {
    LinkedList *linkedList = calloc(1, sizeof(LinkedList));
    linkedList->head = NULL;
    linkedList->tail = NULL;
    return linkedList;
}

typedef struct File {
    long size;
    char* name;
} File;

typedef struct Directory {
    char* name;
    LinkedList *subdirectories;
    LinkedList *files;
    struct Directory *parent;
} Directory;

Directory *new_directory(char *new_name) {
    Directory* new_directory = calloc(1, sizeof(Directory));
    char *name = calloc(strlen(new_name), sizeof(char));
    strncpy(name, new_name, strlen(new_name));
    if (name[strlen(name) - 1] == '\n') {
        name[strlen(name) - 1] = 0;
    }
    new_directory->name = name;
    new_directory->subdirectories = new_list();
    new_directory->files = new_list();
    new_directory->parent = NULL;
    return new_directory;
}

Directory *add_directory(Directory *parent, char *new_name) {
    Directory *directory = new_directory(new_name);
    directory->parent = parent;
    add_element(parent->subdirectories, directory);
}

Directory *find_subdirectory(Directory* parent, char *name) {
    LinkedListElement *element = parent->subdirectories->head;
    while (element) {
        Directory *directory = (Directory*) element->element;
        if (strncmp(directory->name, name, strlen(directory->name)) == 0) {
            return directory;
        }
        element = element->next;
    }

    return NULL;
}

long directory_size(Directory *directory) {
    long result = 0;

    LinkedListElement *element = directory->files->head;
    while (element) {
        File *file = (File*) element->element;
        result += file->size;
        element = element->next;
    }

    element = directory->subdirectories->head;
    while (element) {
        Directory *subdirectory = (Directory*) element->element;
        result += directory_size(subdirectory);
        element = element->next;
    }

    return result;
}

void add_file(Directory *directory, char *name, long size) {
    char *alloced_name = calloc(strlen(name), sizeof(char));
    strncpy(alloced_name, name, strlen(name));
    if (alloced_name[strlen(alloced_name) - 1] == '\n') {
        alloced_name[strlen(alloced_name) - 1] = 0;
    }
    File *file = calloc(1, sizeof(File));
    file->name = alloced_name;
    file->size = size;
    add_element(directory->files, file);
}

void print_directory(Directory *directory, int indentation_level) {
    for (int i = 0; i < indentation_level; i++) {
        printf(" ");
    }
    printf("- %s (size=%d)\n", directory->name, directory_size(directory));

    LinkedListElement *element = directory->subdirectories->head;
    while (element) {
        Directory *subdirectory = (Directory*) element->element;
        print_directory(subdirectory, indentation_level + 2);
        element = element->next;
    }
}

void free_directory(Directory *directory) {
    free(directory->name);

    LinkedListElement *element = directory->files->head;
    while (element) {
        if (element->previous) {
            free(element->previous);
        }
        File *file = (File*) element->element;
        free(file->name);
        free(file);
        element = element->next;
    }
    free(directory->files->tail);
    free(directory->files);

    element = directory->subdirectories->head;
    while (element) {
        if (element->previous) {
            free(element->previous);
        }
        Directory *subdirectory = (Directory*) element->element;
        free_directory(subdirectory);
        element = element->next;
    }
    free(directory->subdirectories->tail);
    free(directory->subdirectories);
    free(directory);
}

long solve_problem(Directory* directory) {
    long result = 0;
    long size = directory_size(directory);
    if (size <= 100000) {
        result += size;
    }

    LinkedListElement *element = directory->subdirectories->head;
    while (element) {
        Directory *subdirectory = (Directory*) element->element;
        long size = solve_problem(subdirectory);
        result += size;
        element = element->next;
    }

    return result;
}

int main() {
    Directory *root = new_directory("/");
    Directory *current_directory = root;

    FILE *fp = fopen("./input.txt", "r");
    char * line = NULL;
    size_t len = 0;
    ssize_t read;

    if (!fp) {
        return 1;
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        if (strncmp(line, "$ cd ", 5) == 0) {
            char *directory_name = line + 5;
            if (strncmp(directory_name, "/", 1) == 0) {
                current_directory = root;
                continue;
            }
            if (strncmp(directory_name, "..", 2) == 0) {
                current_directory = current_directory->parent;
                continue;
            }
            current_directory = find_subdirectory(current_directory, directory_name);
            continue;
        }
        if (strncmp(line, "$ ls", 4) == 0) {
            continue; // nothing to do really
        }
        if (strncmp(line, "dir ", 4) == 0) {
            add_directory(current_directory, line + 4);
            continue;
        }
        char *size = strtok(line, " ");
        char *filename = strtok(NULL, " ");
        long filesize = strtol(size, NULL, 10);
        add_file(current_directory, filename, filesize);
    }

    fclose(fp);
    if (line) {
        free(line);
    }

//    print_directory(root, 0);
    printf("%d\n", solve_problem(root));

    free_directory(root);

    return 0;
}
