module ftn_c
    interface
        subroutine openconnection(gameid, player, sock) bind(c, name='openconnection')
            use         :: iso_c_binding
            implicit none
            integer(c_int), value :: gameid, player
            integer(c_int)        :: sock
        end subroutine
        subroutine send3(s, pos, dir) bind(c, name='send3')
            use         :: iso_c_binding
            implicit none
            integer(c_int), value :: s, pos, dir
        end subroutine
        subroutine receive3(s, pos, dir) bind(c, name='receive3')
            use         :: iso_c_binding
            implicit none
            integer(c_int), value :: s
            integer(c_int)        :: pos, dir
        end subroutine
    end interface
end module ftn_c
