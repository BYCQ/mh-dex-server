from fabric.api import env, settings, task, run, sudo, put, cd, local
from show import show_error, show_ok, show_info
from string import Template
import apt
import git

USER_MAP = {
    'mhxdexa.breakds.org': 'breakds',
    'mhxdex.breakds.org': 'breakds',
    'mhxdexc.breakds.org': 'admin',
}

@task
def install_quicklisp():
    """Install quicklisp if it is not already.
    """
    result = run('ls ~/quicklisp', quiet=True)
    if result.failed:
        run('wget https://beta.quicklisp.org/quicklisp.lisp -O /tmp/quicklisp.lisp')
        put('config-quicklisp.lisp', '/tmp/config-quicklisp.lisp')
        run('sbcl --load /tmp/quicklisp.lisp --load /tmp/config-quicklisp.lisp')
        show_ok('successfully installed and configured "quicklisp".')
    else:
        show_ok('"quicklisp" is already installed.')

@task
def prepare_lisp():
    """Ensure that the lisp environment is ready.
    """
    apt.install('sbcl')
    apt.install('emacs')
    apt.install('slime')
    install_quicklisp()

@task
def update_database():
    """Update the dex database.
    """
    local_checksum = local('md5sum ~/dataset/mhx/mhx.db | sed "s/\\(.\\+\\)\\s.*$/\\1/"', capture=True)
    show_info('local checksum: "%s"' % local_checksum)
    exist = run('ls ~/quicklisp/local-projects/mh-dex-server/data/mhx.db', quiet=True)
    needs_update = False;
    if not exist.failed:
        remote_checksum = run('md5sum ~/quicklisp/local-projects/mh-dex-server/data/mhx.db | sed "s/\\(.\\+\\)\\s.*$/\\1/"',
                              quiet=True)
        show_info('remote_checksum: "%s"' % remote_checksum)
        if remote_checksum != local_checksum:
            needs_update = True
    else:
        needs_update = True

    if not needs_update:
        show_ok('Version match. No need to update database.')
    else:
        show_info('Starting updating the database ...')
        run('rm -rf ~/quicklisp/local-projects/mh-dex-server/data')
        run('mkdir -p ~/quicklisp/local-projects/mh-dex-server/data/items')
        put('~/dataset/mhx/mhx.db', '~/quicklisp/local-projects/mh-dex-server/data/mhx.db')
        put('../data/items/item-icon.lisp', '~/quicklisp/local-projects/mh-dex-server/data/items/item-icon.lisp')
        show_ok('database updated.')

@task
def prepare_repo():
    """Ensure that repo mh-dex-server is cloned and up-to-date.
    """
    with cd('~/quicklisp/local-projects'):
        git.clone_or_pull('realispic2', 'https://github.com/breakds/realispic2')
        git.clone_or_pull('mh-dex-server', 'https://github.com/BYCQ/mh-dex-server')

@task
def update_systemd():
    with open('mh-dex.service') as f:
        d = dict(user=USER_MAP[env.host])
        config_text = Template(f.read()).safe_substitute(d)
    with open('/tmp/mh-dex.service', 'w') as f:
        f.write(config_text)
    put('/tmp/mh-dex.service', '/etc/systemd/system/mh-dex.service', use_sudo=True)
    sudo('systemctl daemon-reload')
    show_ok('systemd Unit file mh-dex.serivce has been updated.')

@task
def compile_and_start():
    """Compile the sbcl image.
    """
    sudo('systemctl stop mh-dex.service', quiet=True)
    run('mkdir -p ~/bin')
    run('rm -rf ~/bin/mh-dex-server')
    with cd('~/bin'):
        run('sbcl --load ~/quicklisp/local-projects/mh-dex-server/src/build.lisp')
    sudo('systemctl start mh-dex.service')
    sudo('systemctl status mh-dex.service')

@task
def deploy():
    """Entry point.
    """
    local('ssh-add ~/.ssh/breakds.ec2.korea.pem')
    prepare_lisp()
    prepare_repo()
    update_database()
    update_systemd()
    compile_and_start()
