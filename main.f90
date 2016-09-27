program main

    use boardclass
    use prec
    use globals
    use ai
    use ftn_c

    implicit none

    type(board) :: game
    type(act) :: move
    integer :: iarg
    CHARACTER(len=32) :: arg
    integer :: gameid, tmpint, socket, ipos, idir
    integer(pi) :: htype
    integer(pi4):: v, alpha, beta, nmoves, rounds, mdepth
    real        :: start, finish, starttot, finishtot, maxt

    ! Default values
    imax = 5
    jmax = 4
    iam = p1
    heis = p2
    htype = 0
    gameid = 11
    big = huge(v)
    mdepth = 10
    sortd = 0

    do iarg = 1, iargc()
        call getarg(iarg, arg)
        if(arg == '-7x6') then
            imax = 7
            jmax = 6
            mdepth = 10
            sortd = 0
        else if(arg == '-white') then
            iam = p1
            heis = p2
        else if(arg == '-black') then
            iam = p2
            heis = p1
        else if(arg(1:5) == '-game') then
            read(arg(6:7),'(I2)') gameid
        else
            write(*,*) 'Invalid argument: ', arg
        end if
    end do

    tmpint = iam
    call openconnection(gameid,tmpint,socket)
    call initialize_board(game)
    call printboard(game)

    rounds = 0
    nmoves = 0
    call cpu_time(starttot)
    maxt = 0
    if(iam.eq.p2) then
        call printroundplayer(rounds, nmoves, heis)
        call receive3(socket, ipos, idir)
        call iposidirtomove(ipos, idir, move, .true.)
        call movepiece(game, move)
        call printboard(game)
        call checkwinner(game)
    end if
    do while(game%winner.eq.empty)

        call printroundplayer(rounds, nmoves, iam)

        call cpu_time(start)
        nnode = 0
        call alphabeta(v, game, -huge(alpha), huge(beta), iam, 0, mdepth, move, htype)
        call cpu_time(finish)
        write(*,'(A, 2I1, A1, /, A, I0, /, A, I0, A, F6.3, A, /)') &
            'Best Move: ', move%i, move%j, move%d, &
            'Heuristic Value: ', v, &
            'Nodes evaluated: ', nnode, '  Time required: ', finish - start, ' seconds'
        if(finish-start.ge.maxt) maxt = finish-start

        call iposidirtomove(ipos, idir, move, .false.)
        call send3(socket, ipos, idir) 
        call movepiece(game, move)
        call printboard(game)
        call checkwinner(game)
        if(game%winner.ne.empty) exit

        call printroundplayer(rounds, nmoves, heis)
        call receive3(socket, ipos, idir)
        call iposidirtomove(ipos, idir, move, .true.)
        call movepiece(game, move)
        call printboard(game)
        call checkwinner2(game)
        if(game%winner.ne.empty) exit

        if(rounds.eq.1000) exit
    end do
    call cpu_time(finishtot)
    print '("Total time = ",f16.3," seconds.")',finishtot-starttot
    print '("Average time / search = ",f6.3," seconds.")', (finishtot-starttot) / rounds
    print '("Max search time = ",f6.3," seconds.")', maxt

    deallocate(game%squares)

    open(unit=40,file='winnings.dat',Access = 'append',Status='old')
    if(game%winner.eq.iam) then
        write(40,'(A,I4,A)') 'I won in ', rounds,' rounds. :)'
        write(*,'(A,I4,A)') 'I won in ', rounds,' rounds. :)'
    endif
    if(game%winner.eq.heis) then
        write(40,'(A,I4,A)') 'I lost in ', rounds,' rounds. :('
        write(*,'(A,I4,A)') 'I lost in ', rounds,' rounds. :('
    endif
    if(game%winner.eq.empty) then
        write(40,'(A,I4,A)') 'It s a tie after ', rounds,' rounds. :\'
        write(*,'(A,I4,A)') 'It s a tie after ', rounds,' rounds. :\'
    endif
    close(40)

end program

subroutine iposidirtomove(ipos, idir, move, rcv)
    use boardclass
    implicit none
    integer :: ipos, idir
    type(act) :: move
    logical :: rcv

    if(rcv) then
        move%i = ipos / 10 
        move%j = mod(ipos,10)
        select case(idir)
            case(1)
                move%d = 'N' 
            case(2)
                move%d = 'E' 
            case(3)
                move%d = 'S'
            case(4)
                move%d = 'W' 
        end select
    else
        ipos = move%i*10 + move%j
        select case(move%d)
            case('N')
                idir = 1
            case('E')
                idir = 2
            case('S')
                idir = 3
            case('W')
                idir = 4
        end select
    end if

    return
end subroutine

subroutine printroundplayer(rounds, nmoves, player)
    use prec
    use globals
    implicit none

    integer :: rounds, nmoves
    integer(pi) :: player

    rounds = 1 + nmoves / 2
    nmoves = nmoves + 1
    write(*,'(/,A,/)') '*************************'
    if(player.eq.p1) then
        write(*,'(A,I0,A)') 'Round: ', rounds, ' Player: White (+)'
    else
        write(*,'(A,I0,A)') 'Round: ', rounds, ' Player: Black (-)'
    end if

end subroutine
