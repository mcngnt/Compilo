// Test : incr var fun

// OK

int* incr(int* addr)
{
	*addr = *addr + 1;
	return addr;
}


int main()
{
	int a;
	a = 0;
	return *incr(&a);
}