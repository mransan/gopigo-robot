
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/fail.h"

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <linux/i2c-dev.h>
#include <errno.h>

#define I2C_ADDR 0x08


value gopigo_init_device(value unit) {
    CAMLparam1(unit);
	
    int fd = -1; 
    char error_buffer[1024] = {0}; 
    
    fd = open("/dev/i2c-1", O_RDWR | O_NONBLOCK);

	if (fd < 0) {
        snprintf(error_buffer, 
                 1024, 
                 "Error opening device /dev/i2c-1, detail: %s", 
                 strerror(errno));
        caml_failwith(error_buffer);
	}

	if (ioctl(fd, I2C_SLAVE, I2C_ADDR) < 0) {
        snprintf(error_buffer, 
                 1024, 
                 "iotcl error, detail: %s",
                 strerror(errno));
        caml_failwith(error_buffer);
	}

    CAMLreturn(Val_int(fd));
}
