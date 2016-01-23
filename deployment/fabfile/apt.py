from fabric.api import settings, task, run, sudo
from show import show_error, show_ok, show_info
import re
import sys

# ==============================
# Apt package management       |
# ==============================

@task
def install_aptitude():
    """Install aptitude if it is not present
    """
    with settings(warn_only=True):
        result = run('which aptitude')
        if result.failed:
            sudo('apt-get -y install aptitude', quiet=True)

@task
def update():
    """Run apt-get update
    """
    with settings(warn_only=True):
        show_info('apt-get updating ...')
        sudo('apt-get update', quiet=True)
        show_ok('apt-get updated.')

@task
def installed(package):
    """Check whether an apt package is installed.
    """
    result = run('aptitude show %s' % package, quiet=True)
    if result.failed:
        show_error('"aptitude show" failed, possibly because the package "%s" does not exist.' % package)
        sys.exit()
    state = re.search('State:\s*(.*installed)', result)
    if state is None:
        show_error('Cannot find "State:" in aptitude show result.')
        sys.exit()
    return state.group(1) == 'installed'

@task
def install(package):
    """Install the package if it is not already installed.
    """
    with settings(warn_only=True):
        if not installed(package):
            show_info('Installing package "%s" ...' % package)
            result = sudo('apt-get -y --force-yes install %s' % package, quiet=True)
            if result.failed:
                show_error('Failed to install package "%s".' % package)
                print result
                sys.exit()
            else:
                show_ok('package "%s" is installed now.' % package)
        else:
            show_ok('"%s" is already installed.' % package)

@task
def install_packages(packages):
    """Install the list of packages.
    """
    for package in packages:
        install(package)
            
        


            
    
    
    
    
        
    
