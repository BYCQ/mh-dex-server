from fabric.api import task, run, cd
from show import show_ok, show_info, show_error
import apt

# ==============================
# git repository management    |
# ==============================

@task
def clone(origin):
    """clone the specified repo into local directory.
    """
    apt.install('git')
    run('git clone %s' % origin)

@task
def clone_or_pull(repo, origin):
    """clone the specified repo from origin if it does not exist yet, or
    just pull to update it.
    """
    apt.install('git')
    result = run('ls %s' % repo, quiet=True)
    if result.failed:
        clone(origin)
        show_ok('cloned %s.' % origin)
    else:
        with cd(repo):
            run('git pull')
        show_ok('successfully updated repo: %s' % repo)
