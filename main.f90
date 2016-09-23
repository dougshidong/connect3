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
    CHARACTER(len=3)  :: io_move
    integer :: gameid, tmpint, socket, ipos, idir
    integer(pi) :: player, htype
    integer(pi4):: v, alpha, beta, rounds, mdepth
    real        :: start, finish, starttot, finishtot, maxt

    big = huge(v)
    imax = 5
    jmax = 4
    iam = p1
    heis = p2
    htype = 0
    mdepth = 10

    do iarg = 1, iargc()
        call getarg(iarg, arg)
        if(arg == '-7x6') then
            imax = 7
            jmax = 6
            mdepth = mdepth - 1
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
    call cpu_time(starttot)
    maxt = 0
    if(iam.eq.p2) then
        player = heis
        rounds = rounds + 1
        write(*,*) 'Player', player
        call receive3(socket, ipos, idir)
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
        call movepiece(game, move)
        call printboard(game)
        call checkwinner(game)
    end if
    do while(game%winner.eq.empty)
        rounds = rounds + 1

        write(*,*) 'ROUND:', rounds
        write(*,*) 'Player', iam
        call cpu_time(start)
        call alphabeta(v, game, -huge(alpha), huge(beta), iam, 0, mdepth, move, htype)
        call cpu_time(finish)
        print '("Time = ",f6.3," seconds.")',finish-start
        write(*,'(A,2I1,A1," Value:",I0," Time: ", f6.3, " seconds")') &
            'Best Move: ', move%i, move%j, move%d, v, finish - start
        if(finish-start.ge.maxt) maxt = finish-start

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
        write(*,*) ipos, idir
        call send3(socket, ipos, idir) 
        call movepiece(game, move)
        call printboard(game)
        call checkwinner(game)
        if(game%winner.ne.empty) exit

        write(*,*) 'Player', heis
        call receive3(socket, ipos, idir)
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
        call movepiece(game, move)
        call printboard(game)
        call checkwinner(game)
        if(game%winner.ne.empty) exit

        if(rounds.eq.100) exit
    end do
    call cpu_time(finishtot)
    print '("Total time = ",f16.3," seconds.")',finishtot-starttot
    print '("Average time / search = ",f6.3," seconds.")', (finishtot-starttot) / rounds
    print '("Max search time = ",f6.3," seconds.")', maxt

    deallocate(game%squares)

    write(*,*) game%winner

    if(game%winner.eq.iam) then
        write(*,'(A,I4,A)') 'I won in ', rounds,' moves. :)'
    endif
    if(game%winner.eq.heis) then
        write(*,'(A,I4,A)') 'I lost in ', rounds,' moves. :('
    endif
    if(game%winner.eq.empty) then
        write(*,'(A,I4,A)') 'It s a tie after ', rounds,' moves. :\'
    endif


end program
