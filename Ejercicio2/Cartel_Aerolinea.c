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
#define MAX_DELAY_PASAJERO 3

// Variables globales para control de acceso
sem_t mutex;           // Protege la variable lectores_activos
sem_t escritura;       // Controla el acceso exclusivo para escritura
sem_t cola;  // "turnstile" para impedir que lectores se cuelen cuando hay escritores esperando

int lectores_activos = 0;

// Estructura para datos del panel (simulación)
typedef struct {
    char info[200];
} PanelVuelos;

PanelVuelos panel;

// Función para inicializar el panel
void inicializar_panel(void) {
    snprintf(panel.info, sizeof(panel.info), "Panel inicializado con información de vuelos");
}

// Función que ejecuta cada pasajero (lector)
// 2) LECTOR (pasajero): pasar por el torniquete al entrar
void* pasajero(void* arg) {
    int id = *((int*)arg);
    free(arg);

    // (opcional) que no sea 0 para no “ganar” siempre: 
    usleep(1000 * (200 + rand() % 600)); // 200–800 ms

    // ---- Turnstile: no permitir colarse si un escritor lo cerró ----
    sem_wait(&cola);
    sem_post(&cola);
    // ---------------------------------------------------------------

    // ENTRADA LECTOR
    sem_wait(&mutex);
    lectores_activos++;
    if (lectores_activos == 1) sem_wait(&escritura);
    sem_post(&mutex);

    printf("🔵 Pasajero %d está mirando el cartel\n", id);
    usleep(1000 * (200 + rand() % 600));

    // SALIDA LECTOR
    sem_wait(&mutex);
    lectores_activos--;
    if (lectores_activos == 0) sem_post(&escritura);
    sem_post(&mutex);

    return NULL;
}

// 3) ESCRITOR (oficinista): cerrar el torniquete antes de pedir el recurso
void* oficinista(void* arg) {
    int id = *((int*)arg);
    free(arg);

    for (int cambio = 1; cambio <= CAMBIOS_POR_OFICINISTA; cambio++) {
        usleep(1000 * (300 + rand() % 900)); // 300–1200 ms, por ejemplo

        // ---- Cerrar turnstile y luego tomar el recurso ----
        sem_wait(&cola);       // bloquea a nuevos lectores para que no se cuelen
        sem_wait(&escritura);  // exclusión total para escribir
        // ---------------------------------------------------

        printf("🔴 Oficinista %d está modificando el cartel (cambio %d/%d)\n",
               id, cambio, CAMBIOS_POR_OFICINISTA);
        usleep(1000 * (300 + rand() % 900));

        snprintf(panel.info, sizeof(panel.info),
                 "Actualización realizada por Oficinista %d (cambio %d)",
                 id, cambio);
        printf("   ✓ Oficinista %d completó el cambio %d/%d\n", id, cambio, CAMBIOS_POR_OFICINISTA);

        // ---- Liberar en orden inverso ----
        sem_post(&escritura);
        sem_post(&cola);
        // ----------------------------------

        usleep(1000 * 100);
    }
    return NULL;
}


int main(void) {
    pthread_t pasajeros[NUM_PASAJEROS];
    pthread_t oficinistas[NUM_OFICINISTAS];

    // Inicializar semilla de números aleatorios
    srand((unsigned)time(NULL));

    // Inicializar semáforos
    sem_init(&mutex, 0, 1);
    sem_init(&escritura, 0, 1);
    sem_init(&cola, 0, 1);     // <-- NUEVO


    // Inicializar el panel
    inicializar_panel();

    printf("═══════════════════════════════════════════════════════\n");
    printf("  SISTEMA DE PANEL DE VUELOS - AEROPUERTO INTERNACIONAL\n");
    printf("═══════════════════════════════════════════════════════\n");
    printf("  Pasajeros: %d | Oficinistas: %d\n", NUM_PASAJEROS, NUM_OFICINISTAS);
    printf("  Cambios por oficinista: %d\n", CAMBIOS_POR_OFICINISTA);
    printf("═══════════════════════════════════════════════════════\n\n");

    // Crear hilos intercaladamente para mejor distribución
    int pasajero_idx = 0;
    int oficinista_idx = 0;
    
    printf("Iniciando sistema...\n\n");
    
    // Crear todos los oficinistas primero
    for (int i = 0; i < NUM_OFICINISTAS; i++) {
        int* id = (int*)malloc(sizeof(int));
        if (!id) { perror("malloc oficinista"); exit(EXIT_FAILURE); }
        *id = i + 1;
        if (pthread_create(&oficinistas[i], NULL, oficinista, id) != 0) {
            perror("pthread_create oficinista");
            exit(EXIT_FAILURE);
        }
    }
    
    // Crear todos los pasajeros
    for (int i = 0; i < NUM_PASAJEROS; i++) {
        int* id = (int*)malloc(sizeof(int));
        if (!id) { perror("malloc pasajero"); exit(EXIT_FAILURE); }
        *id = i + 1;
        if (pthread_create(&pasajeros[i], NULL, pasajero, id) != 0) {
            perror("pthread_create pasajero");
            exit(EXIT_FAILURE);
        }
        // Pequeña pausa para distribuir mejor los pasajeros
        if (i % 10 == 0) {
            usleep(50 * 1000);
        }
    }

    // Esperar a que todos los oficinistas terminen
    for (int i = 0; i < NUM_OFICINISTAS; i++) {
        pthread_join(oficinistas[i], NULL);
    }

    // Esperar a que todos los pasajeros terminen
    for (int i = 0; i < NUM_PASAJEROS; i++) {
        pthread_join(pasajeros[i], NULL);
    }

    // Destruir semáforos
    sem_destroy(&mutex);
    sem_destroy(&escritura);

    printf("\n═══════════════════════════════════════════════════════\n");
    printf("  TODAS LAS OPERACIONES COMPLETADAS EXITOSAMENTE\n");
    printf("═══════════════════════════════════════════════════════\n");

    return 0;
}