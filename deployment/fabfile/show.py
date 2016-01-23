from fabric.colors import green, red, blue

def show_error(message):
    print red('[error]'), message

def show_ok(message):
    print green('[ok]'), message

def show_info(message):
    print blue('[info]'), message


