top = '.'

subdirs = ['report', 'src']


def configure(conf):
    conf.recurse(subdirs)


def build(bld):
    bld.recurse(subdirs)
