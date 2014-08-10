#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#include "app_cfg.h"

#include "interfaces.h"

#define GPIO_PORT				0
#define GPIO_PIN				1

int main(void)
{
	interfaces->core.init(NULL);
	interfaces->gpio.init(GPIO_PORT);
	interfaces->gpio.config(GPIO_PORT, GPIO_PIN, GPIO_PIN, 0, 0);
	while (1)
	{
		interfaces->gpio.set(GPIO_PORT, GPIO_PIN);
		interfaces->gpio.clear(GPIO_PORT, GPIO_PIN);
	}
	return 0;
}
