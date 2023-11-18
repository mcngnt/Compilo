// Test : incr var fun

// GOOD

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