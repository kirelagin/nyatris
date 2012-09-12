def configure(conf):
    conf.load('tex')
    if not conf.env.PDFLATEX:
        conf.fatal('pdflatex is required')
    conf.find_program('epstopdf')


def build(bld):
    epsfigures = ['logics', 'global']
    for name in epsfigures:
        bld(
            rule='${EPSTOPDF} ${SRC} --outfile=${TGT}',
            source='images/{0}.eps'.format(name),
            target='images/{0}-eps-converted-to.pdf'.format(name)
           )


    bld.add_group()


    bld(
        features = 'tex',
        type     = 'pdflatex',
        source   = 'main.latex',
       )
