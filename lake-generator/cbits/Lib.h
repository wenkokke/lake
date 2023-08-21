typedef void *Generator;
typedef char *Value;
typedef unsigned char Bool;

extern void hs_init(int *argc, char **argv[]);
extern void hs_exit(void);

extern Generator lake_generator_new(void);
extern void lake_generator_free(Generator generator);
extern Value lake_generator_value(Generator generator);
extern void lake_generator_value_free(Value value);
extern Bool lake_generator_has_next(Generator generator);
extern Generator lake_generator_next(Generator generator);
