#include <stdio.h>
#include <string.h>

void
transform (const char *productions[], int index, char word[])
{
  int i;
  int append = (word[0] == '1');
  for (i = 0; word[i + 1] != '\0'; i++)
    word[i] = word[i + 1];
  if (append)
    {
      for (const char *p = productions[index]; *p != '\0'; p++, i++)
        word[i] = *p;
    }
  word[i] = '\0';
}

int
main ()
{
  int max_steps = 20;
  const char *productions[] = { "110", "01", "0000", NULL };
  int length = 0;
  for (const char **p = productions; *p != NULL; p++, length++);
  char word[128] = "1";

  for (int i = 0, index = 0; i < max_steps; i++)
    {
      if (word[0] != '\0')
        {
          for (int j = 0; j < i; j++) putchar (' ');
          puts (word);
          transform (productions, index, word);
          index = (index + 1) % length;
        }
      else
        {
          for (int j = 0; j < i; j++) putchar (' ');
          puts ("(Halt)");
          break;
        }
    }
  return 0;
}
