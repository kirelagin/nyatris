def configure(conf):
    conf.recurse('report')
    conf.recurse('src')


def build(bld):
    bld.recurse('report')
    bld.recurse('src')
