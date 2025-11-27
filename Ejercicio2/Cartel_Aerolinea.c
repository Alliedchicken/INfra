#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <time.h>

#define NUM_PASAJEROS 100
#define NUM_OFICINISTAS 5
#define CAMBIOS_POR_OFICINISTA 3
#define MAX_DELAY_OFICINISTA 5
#define MAX_DELAY_
sem_t mutex;
sem_t escritura;
sem_t cola;

int lectores_activos = 0;

typedef struct {
    char info[200];
} PanelVuelos;

PanelVuelos panel;

void inicializar_panel(void) {
    snprintf(panel.info, sizeof(panel.info), "Panel inicializado con información de vuelos");
}

void* pasajero(void* arg) {
    int id = *((int*)arg);
    free(arg);

    usleep(1000 * (200 + rand() % 600));

    sem_wait(&cola);
    sem_post(&cola);

    sem_wait(&mutex);
    lectores_activos++;
    if (lectores_activos == 1) sem_wait(&escritura);
    sem_post(&mutex);

    printf("🔵 Pasajero %d está mirando el cartel\n", id);
    usleep(1000 * (200 + rand() % 600));

    sem_wait(&mutex);
    lectores_activos--;
    if (lectores_activos == 0) sem_post(&escritura);
    sem_post(&mutex);

    return NULL;
}

void* oficinista(void* arg) {
    int id = *((int*)arg);
    free(arg);

    for (int cambio = 1; cambio <= CAMBIOS_POR_OFICINISTA; cambio++) {
        usleep(1000 * (300 + rand() % 900));

        sem_wait(&cola);
        sem_wait(&escritura);

        printf("🔴 Oficinista %d está modificando el cartel (cambio %d/%d)\n",
               id, cambio, CAMBIOS_POR_OFICINISTA);
        usleep(1000 * (300 + rand() % 900));

        snprintf(panel.info, sizeof(panel.info),
                 "Actualización realizada por Oficinista %d (cambio %d)",
                 id, cambio);
        printf("   ✓ Oficinista %d completó el cambio %d/%d\n", id, cambio, CAMBIOS_POR_OFICINISTA);

        sem_post(&escritura);
        sem_post(&cola);

        usleep(1000 * 100);
    }
    return NULL;
}


int main(void) {
    pthread_t pasajeros[NUM_PASAJEROS];
    pthread_t oficinistas[NUM_OFICINISTAS];

    srand((unsigned)time(NULL));

    sem_init(&mutex, 0, 1);
    sem_init(&escritura, 0, 1);
    sem_init(&cola, 0, 1);


    inicializar_panel();

    printf("═══════════════════════════════════════════════════════\n");
    printf("  SISTEMA DE PANEL DE VUELOS - AEROPUERTO INTERNACIONAL\n");
    printf("═══════════════════════════════════════════════════════\n");
    printf("  Pasajeros: %d | Oficinistas: %d\n", NUM_PASAJEROS, NUM_OFICINISTAS);
    printf("  Cambios por oficinista: %d\n", CAMBIOS_POR_OFICINISTA);
    printf("═══════════════════════════════════════════════════════\n\n");

    int pasajero_idx = 0;
    int oficinista_idx = 0;
    
    printf("Iniciando sistema...\n\n");
    
    for (int i = 0; i < NUM_OFICINISTAS; i++) {
        int* id = (int*)malloc(sizeof(int));
        if (!id) { perror("malloc oficinista"); exit(EXIT_FAILURE); }
        *id = i + 1;
        if (pthread_create(&oficinistas[i], NULL, oficinista, id) != 0) {
            perror("pthread_create oficinista");
            exit(EXIT_FAILURE);
        }
    }
    
    for (int i = 0; i < NUM_PASAJEROS; i++) {
        int* id = (int*)malloc(sizeof(int));
        if (!id) { perror("malloc pasajero"); exit(EXIT_FAILURE); }
        *id = i + 1;
        if (pthread_create(&pasajeros[i], NULL, pasajero, id) != 0) {
            perror("pthread_create pasajero");
            exit(EXIT_FAILURE);
        }
        if (i % 10 == 0) {
            usleep(50 * 1000);
        }
    }

    for (int i = 0; i < NUM_OFICINISTAS; i++) {
        pthread_join(oficinistas[i], NULL);
    }

    for (int i = 0; i < NUM_PASAJEROS; i++) {
        pthread_join(pasajeros[i], NULL);
    }

    sem_destroy(&mutex);
    sem_destroy(&escritura);

    printf("\n═══════════════════════════════════════════════════════\n");
    printf("  TODAS LAS OPERACIONES COMPLETADAS EXITOSAMENTE\n");
    printf("═══════════════════════════════════════════════════════\n");

    return 0;
}
