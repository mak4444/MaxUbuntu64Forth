
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <ctype.h>
#include <termios.h>
#include <sys/types.h>
#include <sys/mman.h>

#include <readline/readline.h>
#include <readline/history.h>


//#include "glue.h"

#ifndef S_IREAD 
#define S_IREAD O_RDONLY
#endif
#ifndef  S_IWRITE
#define S_IWRITE O_WRONLY
#endif

#define FATAL do { fprintf(stderr, "Error at line %d, file %s (%d) [%s]\n", \
  __LINE__, __FILE__, errno, strerror(errno)); exit(1); } while(0)

char key_query_char = 0;

int start4th_m();

struct termios t1, t2;
struct termios t1, t2;

int C_KEY()
{
  /* stack: ( -- n | wait for keypress and return key code ) */

  char ch;
  int cn;

  if (key_query_char)
    {
      ch = key_query_char;
      key_query_char = 0;
    }
  else
    {
      tcgetattr(0, &t1);
      t2 = t1;
      t2.c_lflag &= ~ICANON;
      t2.c_lflag &= ~ECHO;
//      t2.c_lflag &= ~ISIG;
      t2.c_lflag |= ISIG;
      t2.c_cc[VMIN] = 1;
      t2.c_cc[VTIME] = 0;
      tcsetattr(STDIN_FILENO, TCSANOW, &t2);

	cn = getc(stdin);

      tcsetattr(STDIN_FILENO, TCSANOW, &t1);
    }
 
  return cn;
}

int C_KEYQUERY ()
{
  /* stack: ( a -- b | return true if a key is available ) */

  char ch = 0;
  int chq;
  struct termios t1, t2;

  if (key_query_char)  return -1;

      tcgetattr(0, &t1);
      t2 = t1;
      t2.c_lflag &= ~ICANON;
      t2.c_lflag &= ~ECHO;
      t2.c_cc[VMIN] = 0;
      t2.c_cc[VTIME] = 0;
      tcsetattr(0, TCSANOW, &t2);

      chq = read(0, &ch, 1) ? -1 : 0;
      if (ch) key_query_char = ch;  

      tcsetattr(0, TCSANOW, &t1);

  return chq;
}      

long long L_ACCEPT (char *cp, long long n1 )
{  struct termios t1, t2;
  int n2;
  tcgetattr(0, &t1);
  t2 = t1;
  t2.c_lflag |= ICANON;
  t2.c_lflag |= ECHO;
  t2.c_lflag |= PENDIN;

  t2.c_cc[VMIN] = 1;
  t2.c_cc[VTIME] = 0;
  tcsetattr(0, TCSANOW, &t2);
   n2 = read (0, cp, n1);
  tcsetattr(0, TCSANOW, &t1);
  return n2;
}

/* A static variable for holding the line. */
static char *line_read = (char *)NULL;
// http://oco.org.ua/sp-forth-%D0%B2-linux
/* Read a string, and return a pointer to it.  Returns NULL on EOF. */
char *
rl_gets ()
{
  /* If the buffer has already been allocated, return the memory
     to the free pool. */
  if (line_read)
    {
      free (line_read);
      line_read = (char *)NULL;
    }

  /* Get a line from the user. */
  line_read = readline ("");

  /* If the line has any text in it, save it on the history. */
  if (line_read && *line_read)
    add_history (line_read);

  return (line_read);
}

char file_history[128];
char file_historyyy[128];

long long CC_ACCEPT (char *cp, long long n1 )
{
	char * zzz;
	int res=0;
        zzz=rl_gets();
	write_history(file_history);
        while( (*zzz!=0) & (n1!=0) )
        { cp[res]=*zzz; res++; zzz++;n1--; }
	cp[res]='\n';
	return res;
}

int
set_interface_attribs (int fd, int speed, int parity)
{
        struct termios tty;
        if (tcgetattr (fd, &tty) != 0)
        {
                return -1;
        }

        cfsetospeed (&tty, speed);
        cfsetispeed (&tty, speed);

        tty.c_cflag = (tty.c_cflag & ~CSIZE) | CS8;     // 8-bit chars
        // disable IGNBRK for mismatched speed tests; otherwise receive break
        // as \000 chars
        tty.c_iflag &= ~IGNBRK;         // disable break processing
        tty.c_lflag = 0;                // no signaling chars, no echo,
                                        // no canonical processing
        tty.c_oflag = 0;                // no remapping, no delays
        tty.c_cc[VMIN]  = 0;            // read doesn't block
        tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

        tty.c_iflag &= ~(IXON | IXOFF | IXANY); // shut off xon/xoff ctrl

        tty.c_cflag |= (CLOCAL | CREAD);// ignore modem controls,
                                        // enable reading
        tty.c_cflag &= ~(PARENB | PARODD);      // shut off parity
        tty.c_cflag |= parity;
        tty.c_cflag &= ~CSTOPB;
        tty.c_cflag &= ~CRTSCTS;

        if (tcsetattr (fd, TCSANOW, &tty) != 0)
        {
//                error_message ("error %d from tcsetattr", errno);
                printf ("error %d from tcsetattr", errno);
                return -1;
        }
        return 0;
}

int
set_blocking (int fd, int should_block)
{
        struct termios tty;
        memset (&tty, 0, sizeof tty);
        if (tcgetattr (fd, &tty) != 0)
        {
                return 1;
        }

        tty.c_cc[VMIN]  = should_block ? 1 : 0;
        tty.c_cc[VTIME] = 5;            // 0.5 seconds read timeout

        if (tcsetattr (fd, TCSANOW, &tty) != 0)
        {
                printf ("error %d setting term attributes", errno);
                return 1;
        }
	 return 0; 
}

int
serial_set(int fd)
{
	if( set_interface_attribs (fd, B115200, 0))  // set speed to 115,200 bps, 8n1 (no parity)
	{ return 1; }
	set_blocking (fd, 0);                // set no blocking
	{ return 1; }
	 return 0; 
}

void HHDOTC(int xx)
{
  printf("H=%x\n",xx);

}

long long RWGet()
{  return O_RDWR ;
}
long long ROGet()
{  return O_RDONLY ;
 }
long long WOGet()
{  return O_WRONLY ;
}

long long O_CREATGet() { return O_CREAT | O_TRUNC ; }

long long LOPEN(const char *pathname, long flags)
{
// return open(pathname, flags,S_IREAD | S_IWRITE);
// return open(pathname, flags,-1);
 return open(pathname, flags,S_IRWXU | S_IRWXG );

}

long long	smtell(long long _fd)
{ return lseek(_fd, (off_t) 0, SEEK_END); }

long long	mtell(long long _fd)
{ return lseek(_fd, (off_t) 0, SEEK_CUR); }

long long	rmlseek(long long _fd, long long off1)
{ return lseek(_fd, (off_t) off1, SEEK_SET); }

   char * ARGV1;

char* LARGV1() {return ARGV1;}


void c_test()
{
  write(1,"qwerty",6);
}

long long c_div(long long aa, long long bb)
{ return aa/bb;
}


int main(int argc, char **argv) {
    int fd;
    char * buff;
   ARGV1=0;

	if(argc > 1)
	{   ARGV1=argv[1];
	}

//	buff=malloc(99);	free(buff);

	printf("\nARG =\t%s \n", ARGV1);


	snprintf(file_history, sizeof(file_history), "%s/.4th_history", getenv("HOME"));
	read_history(file_history);

	start4th_m();
	
	printf ("proto4th_m exit ok\n");
	exit(0);

    return 0;
}

