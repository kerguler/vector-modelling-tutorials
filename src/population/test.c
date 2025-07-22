#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "population.h"

void run_det(int did, int id, char distr, double *par) {
    char arbiters[2] = {distr, STOP};
    population pop = spop2_init(arbiters, DETERMINISTIC);

    number key[1] = {numZERO};
    number num = {.d=10.0};
    spop2_add(pop, key, num);

    number sz, cm;
    int i;
    for (i=1; i<50; i++) {
        spop2_step(pop, par, &sz, &cm, 0);
    }
}

void run_stoch(int did, int id, char distr, double *par) {
    char arbiters[2] = {distr, STOP};
    population pop = spop2_init(arbiters, STOCHASTIC);

    number key[1] = {numZERO};
    number num = {.i=10};
    spop2_add(pop, key, num);

    number sz, cm;
    int i;
    for (i=1; i<50; i++) {
        spop2_step(pop, par, &sz, &cm, 0);
    }
}

int main(int attr, char *avec[]) {
    spop2_random_init();

    double par[2] = {20.0, 10.0};

    run_det(0, 0, ACC_ERLANG, par);
    run_stoch(0, 1, ACC_ERLANG, par);

    printf("Population package successfully installed!\n");

    return 0;
}
