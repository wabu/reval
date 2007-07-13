require 'down'

lambda do |r|
    path = File.dirname(@filename)
    tip = `cd '#{path}'; hg head`.split[1].split(':')[1]
    unless r.omega.empty?
        r.type, *snap = File.join(r.omega).split('.').reverse
        snap = snap.reverse.join('.')
        throw :download, `cd '#{path}'; hg archive -p #{snap} -r '#{snap.sub(/reval-/,'')}' -t '#{r.type}' -`
    end
    r.join(
        r.small(" Check it out, download the ",
             r.a('/pro/reval/'+tip.sub(/^/,'reval-')+'.tgz', 'latest tip'),
            " or use #{r.a('/pro/hg/reval', 'hg-mercurial')} to get it."),
        r.h4("reval - a relational algebra evaluater"),
        r.p("reval is a leazy relational algebra interpreter written in haskell. You can get more
             info inside the source or the generated doc files:"),
        r.ul(
            r.li( r.a("/pro/hg/reval/file/", "browse") + " the tip"),
            *Dir[File.join(path,'docs','*.html')].map do |f|
                f.sub!(/#{path}/,'')
                r.li(r.a("/pro/hg/reval/raw-file/#{tip}/#{f}",f))
            end
        )
    )
end
