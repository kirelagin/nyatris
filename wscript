def configure(conf):
    conf.load('tex')
    if not conf.env.PDFLATEX:
        conf.fatal('pdflatex is required')
    conf.find_program('epstopdf', var='EPSTOPDF')


def build(bld):
    def build_eps(*names):
        for name in names:
            bld(rule='${EPSTOPDF} ${SRC} --outfile=${TGT}', source='images/{0}.eps'.format(name), target='images/{0}-eps-converted-to.pdf'.format(name))

    build_eps('logics', 'global')
    bld.add_group()
    bld(
        features = 'tex',
        type     = 'pdflatex',
        source   = 'main.latex',
        deps     = ['images/logics.eps'],
       )
