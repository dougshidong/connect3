program main

    use boardclass
    use prec
    use globals
    use ai

    implicit none

    type(board) :: game
    type(act) :: move
    integer :: iarg
    CHARACTER(len=32) :: arg
    integer(pi) :: player, htype
    integer(pi4):: v, alpha, beta, rounds, mdepth
    real        :: start, finish, starttot, finishtot, maxt

    big = huge(v)
    imax = 5
    jmax = 4
    iam = p1
    heis = p2
    write(*,*) iargc()

    do iarg = 1, iargc()
        call getarg(iarg, arg)
        if(arg == '-7x6') then
            imax = 7
            jmax = 6
        else if(arg == '-white') then
            write(*,*) 'White Player Selected'
        else if(arg == '-black') then
            iam = p2
            heis = p1
            write(*,*) 'Black Player Selected'
        else
            write(*,*) 'Invalid argument: ', arg
        end if
    end do

    call initialize_board(game)
    call printboard(game)

    rounds = 0
    player = heis
    if(iam.eq.p2) then
        read(*,'(2I1,A1)') move%i,move%j,move%d
        call movepiece(game, move)
        call printboard(game)
        rounds = rounds + 1
    end if

    call cpu_time(starttot)
    maxt = 0
    do while(game%winner.eq.empty)
        rounds = rounds + 1
!       read(*,'(2I1,A1)') move%i,move%j,move%d
        if(player.eq.iam) then
            player = heis
            mdepth = 11
            htype  = 0
        else
            player = iam
            mdepth = 11
            htype  = 0
        end if
        write(*,*) 'round:', rounds, 'player', player
        call cpu_time(start)
        call alphabeta(v, game, -huge(alpha), huge(beta), player, 0, mdepth, move, htype)
        call cpu_time(finish)
        print '("Time = ",f6.3," seconds.")',finish-start
        write(*,'(A,2I1,A1," Value:",I0," Time: ", f6.3, " seconds")') &
            'Best Move: ', move%i, move%j, move%d, v, finish - start
        if(finish-start.ge.maxt) maxt = finish-start
        call movepiece(game, move)
        call printboard(game)
        call checkwinner(game)
        if(rounds.eq.200) exit
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
