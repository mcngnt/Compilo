
int print_int(int t)
{
    if(t <= 9)
    {
        putc('0' + t);
        return;
    }
    print_int(t/10);
    putc('0' + (t % 10));
    return;
}

int print_int_nl(int t)
{
    if(t < 0)
    {
        t = -t;
        putc('-');
    }
    print_int(t);
    putc('\n');
    return;
}


int main()
{
	int a;
    int b;
    int c;
    int d;

     puts("Mod test : \n");

	a = -10 % 3;
    b = 9 % -5;
    c = -4567 % -123;
    d = 1234 % 123;
    print_int_nl(a);
    print_int_nl(b);
    print_int_nl(c);
    print_int_nl(d);

    puts("Divide test : \n");

    print_int_nl(134 / 10);
    print_int_nl(-134 / 10);
    print_int_nl(134 / -10);
    print_int_nl(-134 / -10);

    puts("Mutl test : \n");

    print_int_nl(134 * 10);
    print_int_nl(-134 * 10);
    print_int_nl(134 * -10);
    print_int_nl(-134 * -10);

	return;
}