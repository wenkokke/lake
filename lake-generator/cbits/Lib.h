typedef void *HsGenerator;
typedef unsigned char HsBool;

extern void hs_init(int *argc, char **argv[]);
extern void hs_exit(void);

extern HsGenerator lake_generator_new(void);
extern void lake_generator_free(HsGenerator generator);
extern char *lake_generator_value(HsGenerator generator);
extern HsBool lake_generator_has_next(HsGenerator generator);
extern HsGenerator lake_generator_next(HsGenerator generator);
