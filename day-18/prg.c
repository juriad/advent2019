#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>
#include <stdint.h>

#define BIT_GET(a, i) (((a) >> (i)) & 1)
#define BIT_SET(a, i) ((a) | (1 << (i)))

typedef struct Point {
    int x;
    int y;
} Point;

typedef struct Map {
    int height;
    int width;

    char * data;
    char ** rows;

    int keyCount;
    Point * keys;
    Point * doors;
} Map;

Point findOnMap(Map map, char feature) {
    for (int y  = 0; y < map.height; y++) {
        for (int x = 0; x < map.width; x++) {
            if (map.rows[y][x] == feature) {
                return (Point) {x, y};
            }
        }
    }
    return (Point) { -1, -1};
}

Map readMap(char * fileName) {
    FILE *f = fopen(fileName, "r");
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *data = malloc(fsize + 1);
    fread(data, 1, fsize, f);
    fclose(f);

    data[fsize] = 0;

    int width = 0;
    int height = 0;
    for (int i = 0; i <= fsize; i++) {
        if (data[i] == 10) {
            height++;
        } else if (height == 0) {
            width++;
        }
    }

    char ** rows = malloc(sizeof(*rows) * height);
    for (int i = 0; i < height; i++) {
        rows[i] = data + i * (width + 1);
    }

    Map proto = (Map) { height, width, data, rows };

    Point entrance = findOnMap(proto, '@');

    int keyCount = 0;
    while (keyCount < 26) {
        if (findOnMap(proto, 97 + keyCount).x >= 0) {
            keyCount++;
        } else {
            break;
        }
    }

    Point * keys = malloc(sizeof*(keys) * keyCount + 4);
    Point * doors = malloc(sizeof*(doors) * keyCount);
    for (int i = 0; i < keyCount; i++) {
        keys[i] = findOnMap(proto, 97 + i);
        doors[i] = findOnMap(proto, 65 + i);
    }
    keys[keyCount] = entrance;

    return (Map) { height, width, data, rows, keyCount, keys, doors };
}

bool canGo(char c, uint64_t holding) {
    if (c == '.' || c == '@' || (c >= 97 && c <= 122)) {
        return true;
    } else if (c == '#') {
        return false;
    } else if (c >= 65 && c <= 90) {
        return BIT_GET(holding, c - 65);
    } else {
        printf("Unknown character %c\n", c);
        exit(1);
    }
}

typedef struct State {
    int keyStartCount;
    int keyStarts[4];

    int keyCount;
    uint64_t holding;
    int holdingCount;

    int distance;
} State;

typedef struct DistanceWithOrigin {
    int distance;
    int origin;
} DistanceWithOrigin;

void doWithQueue(Map map, State state, DistanceWithOrigin ** dds) {
    Point * queue = malloc(map.width * map.height * sizeof(*queue));
    int qsize = 0;
    for (int i = 0; i < state.keyStartCount; i++) {
        Point start = map.keys[state.keyStarts[i]];
//        printf("Enqueued %d %d\n", start.x, start.y);
        queue[qsize++] = start;
    }
    int qptr = 0;

    int found = 0;
    while (qptr < qsize) {
        Point p = queue[qptr++];

        if (map.rows[p.y][p.x] >= 97) {
            found++;
        }

        if (found >= map.keyCount) {
            break;
        }

        int d = dds[p.y][p.x].distance;
        int o = dds[p.y][p.x].origin;
//        printf("Current %d %d dist %d (%c)\n", p.x, p.y, d, map.rows[p.y][p.x]);

        Point n;

        n = (Point) {p.x, p.y-1};
        if (dds[n.y][n.x].distance == -1 && canGo(map.rows[n.y][n.x], state.holding)) {
        // printf("Neighbor %d %d\n", n.x, n.y);
            dds[n.y][n.x].distance = d + 1;
            dds[n.y][n.x].origin = o;
            queue[qsize++] = n;
        }

        n = (Point) {p.x, p.y+1};
        if (dds[n.y][n.x].distance == -1 && canGo(map.rows[n.y][n.x], state.holding)) {
        // printf("Neighbor %d %d\n", n.x, n.y);
            dds[n.y][n.x].distance = d + 1;
            dds[n.y][n.x].origin = o;
            queue[qsize++] = n;
        }

        n = (Point) {p.x-1, p.y};
        if (dds[n.y][n.x].distance == -1 && canGo(map.rows[n.y][n.x], state.holding)) {
        // printf("Neighbor %d %d\n", n.x, n.y);
            dds[n.y][n.x].distance = d + 1;
            dds[n.y][n.x].origin = o;
            queue[qsize++] = n;
        }

        n = (Point) {p.x+1, p.y};
        if (dds[n.y][n.x].distance == -1 && canGo(map.rows[n.y][n.x], state.holding)) {
        // printf("Neighbor %d %d\n", n.x, n.y);
            dds[n.y][n.x].distance = d + 1;
            dds[n.y][n.x].origin = o;
            queue[qsize++] = n;
        }
    }

    free(queue);
 }

DistanceWithOrigin * distancesToKeys(Map map, State state) {
    DistanceWithOrigin * ds = malloc(map.height * map.width * sizeof(*ds));
    DistanceWithOrigin ** dds = malloc(map.height * sizeof(*dds));
    for (int i = 0; i < map.height; i++) {
        dds[i] = ds + i * map.width;
        for (int j = 0; j < map.width; j++) {
            dds[i][j].distance = -1;
            dds[i][j].origin = -1;
        }
    }

    for (int i = 0; i < state.keyStartCount; i++) {
        Point start = map.keys[state.keyStarts[i]];
//        printf ("Starts %d %d %d\n", start.x, start.y, i);
        dds[start.y][start.x].distance = 0;
        dds[start.y][start.x].origin = i;
    }
    doWithQueue(map, state, dds);

    DistanceWithOrigin * distances = malloc(sizeof(*distances) * state.keyCount);
    for (int i = 0; i < state.keyCount; i++) {
        Point t = map.keys[i];
        distances[i] = dds[t.y][t.x];
    }

    free(dds);
    free(ds);

    return distances;
}

typedef struct DOI {
    int distance;
    int origin;
    int index;
} DOI;

int cmpByValue(const void * doi1, const void * doi2) {
    return ((DOI *) doi1)->distance - ((DOI *) doi2)->distance;
}

typedef struct HashMap {
    int bits1;
    int cap1;
    int bits2;
    int cap2;
    int bits3;
    int cap3;
    uint16_t *** values;
} HashMap;

HashMap newHashMap(int bits) {
    int bits1 = bits / 3;
    int bits2 = bits / 3;
    int bits3 = bits - bits1 - bits2;

    int cap1 = (1 << bits1);
    int cap2 = (1 << bits2);
    int cap3 = (1 << bits3);

    uint16_t *** values = malloc(cap1 * sizeof(*values));
    for (int i = 0; i < cap1; i++) {
        values[i] = NULL;
    }

    return (HashMap) { bits1, cap1, bits2, cap2, bits3, cap3, values };
}

void hashPut(HashMap hashMap, uint64_t key, uint16_t value) {
    uint64_t k1 = key % hashMap.cap1;
    uint64_t k2 = (key / hashMap.cap1) % hashMap.cap2;
    uint64_t k3 = (key / hashMap.cap1) / hashMap.cap2;

    uint16_t ** l1 = hashMap.values[k1];
    if (l1 == NULL) {
        l1 = malloc(hashMap.cap2 * sizeof(*l1));
        for (int i = 0; i < hashMap.cap2; i++) {
            l1[i] = 0;
        }
        hashMap.values[k1] = l1;
    }
    uint16_t * l2 = l1[k2];
    if (l2 == NULL) {
        l2 = malloc(hashMap.cap3 * sizeof(*l2));
        for (int i = 0; i < hashMap.cap3; i++) {
            l2[i] = 0;
        }
        l1[k2] = l2;
    }
    l2[k3] = value;
}

uint16_t hashGet(HashMap hashMap, uint64_t key) {
    uint64_t k1 = key % hashMap.cap1;
    uint64_t k2 = (key / hashMap.cap1) % hashMap.cap2;
    uint64_t k3 = (key / hashMap.cap1) / hashMap.cap2;

    uint16_t ** l1 = hashMap.values[k1];
    if (l1 == NULL) {
        return 0;
    }
    uint16_t * l2 = l1[k2];
    if (l2 == NULL) {
        return 0;
    }
    return l2[k3];
}

uint64_t hash(State state) {
    uint64_t h = state.holding;
    for (int i = 0; i < state.keyStartCount; i++) {
        h |= ((uint64_t)state.keyStarts[i]) << (state.keyCount + i * 5);
    }
    return h;
}

int findPath(Map map, State state, int limit, HashMap hashMap) {
    uint64_t h = hash(state);

    uint64_t prev = hashGet(hashMap, h);
//    printf("Prev %u, hash %llu, distance %u\n", prev, h, state.distance);

    if (prev > 0 && prev <= state.distance) {
//        printf("Found before.\n");
        return INT_MAX;
    } else {
        hashPut(hashMap, h, state.distance);
    }

    if (state.holdingCount == state.keyCount) {
//        printf("Have all at distance %d\n", state.distance);
        return state.distance;
    }

//    printf("At %d %d\n", map.keys[state.keyStarts[0]].x, map.keys[state.keyStarts[0]].y);

    DistanceWithOrigin * distances = distancesToKeys(map, state);
    DOI * dois = malloc(map.keyCount * sizeof(*dois));
    for (int i = 0; i < map.keyCount; i++) {
        dois[i].distance = distances[i].distance;
        dois[i].origin = distances[i].origin;
        dois[i].index = i;
    }

    qsort(dois, map.keyCount, sizeof(*dois), cmpByValue);

    for (int j = 0; j < map.keyCount; j++) {
//        printf("Distance to %c is %d from %d\n", dois[j].index + 97, dois[j].distance, dois[j].origin);
    }

    int p = 0;
    for (int j = 0; j < map.keyCount; j++) {
        int i = dois[j].index;
        int o = dois[j].origin;
        int d = dois[j].distance;
        p++;
        if (d > 0 && !BIT_GET(state.holding, i)) {
            State nextState = (State) {
                state.keyStartCount, {
                    o == 0 ? i : state.keyStarts[0],
                    o == 1 ? i : state.keyStarts[1],
                    o == 2 ? i : state.keyStarts[2],
                    o == 3 ? i : state.keyStarts[3]
                },
                state.keyCount, BIT_SET(state.holding, i), state.holdingCount + 1,
                state.distance + d
            };

            if (state.holdingCount <= 4) {
//                printf("%d Visiting %d/%d\n", state.holdingCount, p, map.keyCount);
            }

            int found = findPath(map, nextState, limit, hashMap);
            if (found < limit) {
                printf("Found better distance %d\n", found);
                limit = found;
            }
        }
    }

    free(dois);
    free(distances);
    return limit;
}

int run(Map map, State state) {
    int bits = map.keyCount + state.keyStartCount * 5;
    int distance = findPath(map, state,  INT_MAX, newHashMap(bits));
    return distance;
}

void patchMap(Map map) {
    Point e = map.keys[map.keyCount];

    map.rows[e.y-1][e.x-1] = '@';
    map.keys[map.keyCount + 0] = (Point) {e.x-1, e.y-1};
    map.rows[e.y-1][e.x+1] = '@';
    map.keys[map.keyCount + 1] = (Point) {e.x+1, e.y-1};
    map.rows[e.y+1][e.x+1] = '@';
    map.keys[map.keyCount + 2] = (Point) {e.x+1, e.y+1};
    map.rows[e.y+1][e.x-1] = '@';
    map.keys[map.keyCount + 3] = (Point) {e.x-1, e.y+1};

    map.rows[e.y-1][e.x] = '#';
    map.rows[e.y][e.x-1] = '#';
    map.rows[e.y][e.x+1] = '#';
    map.rows[e.y+1][e.x] = '#';

    map.rows[e.y][e.x] = '#';
}

int main(int argc, char** argv) {
    Map map = readMap(argv[1]);

    State state1 = (State) {
        1, {map.keyCount, 0, 0, 0},
        map.keyCount, 0, 0,
        0
    };

    printf("Simple: %d\n", run(map, state1));

    patchMap(map);
    State state2 = (State) {
       4, {map.keyCount + 0, map.keyCount + 1, map.keyCount + 2, map.keyCount + 3},
       map.keyCount, 0, 0,
       0
   };

   printf("Patched: %d\n", run(map, state2));

   return 0;
}
