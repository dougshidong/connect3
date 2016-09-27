module boardclass

    use prec
    use globals
    implicit none

    integer(pi)  :: imax, jmax

    type :: board
        integer(pi) :: winner, turn
        integer(pi4) :: depth
        type(board), pointer :: children(:)
        integer(pi), allocatable :: squares(:,:)
        ! pieces(a, b, c)
        ! a = coordinate : i and j positions
        ! b = ipiece = 1:4
        ! c = player 1 and 2
        integer(pi) :: pieces(2,4,2)
    end type board

    type :: act
        integer(pi) :: i, j
        character(len=1) :: d
    end type act

    contains

    subroutine initialize_board(root)
        implicit none

        type (board) :: root

        allocate(root%squares(imax, jmax))
        root%squares = empty
        root%winner = empty
        root%depth  = 0
        root%turn   = p1

        if(imax.eq.5 .and. jmax.eq.4) then
            root%squares(1,1) = p1
            root%squares(1,2) = p2
            root%squares(1,3) = p1
            root%squares(1,4) = p2

            root%squares(5,1) = p2
            root%squares(5,2) = p1
            root%squares(5,3) = p2
            root%squares(5,4) = p1

            root%pieces(1,1,1) = 1
            root%pieces(2,1,1) = 1
            root%pieces(1,2,1) = 1
            root%pieces(2,2,1) = 3
            root%pieces(1,3,1) = 5
            root%pieces(2,3,1) = 2
            root%pieces(1,4,1) = 5
            root%pieces(2,4,1) = 4

            root%pieces(1,1,2) = 1
            root%pieces(2,1,2) = 2
            root%pieces(1,2,2) = 1
            root%pieces(2,2,2) = 4
            root%pieces(1,3,2) = 5
            root%pieces(2,3,2) = 1
            root%pieces(1,4,2) = 5
            root%pieces(2,4,2) = 3
        else if(imax.eq.7 .and. jmax.eq.6) then
            root%squares(2,2) = p1
            root%squares(2,3) = p2
            root%squares(2,4) = p1
            root%squares(2,5) = p2

            root%squares(6,2) = p2
            root%squares(6,3) = p1
            root%squares(6,4) = p2
            root%squares(6,5) = p1

            root%pieces(1,1,1) = 2
            root%pieces(2,1,1) = 2
            root%pieces(1,2,1) = 2
            root%pieces(2,2,1) = 4
            root%pieces(1,3,1) = 6
            root%pieces(2,3,1) = 3
            root%pieces(1,4,1) = 6
            root%pieces(2,4,1) = 5

            root%pieces(1,1,2) = 2
            root%pieces(2,1,2) = 3
            root%pieces(1,2,2) = 2
            root%pieces(2,2,2) = 5
            root%pieces(1,3,2) = 6
            root%pieces(2,3,2) = 2
            root%pieces(1,4,2) = 6
            root%pieces(2,4,2) = 4
        else
            write(*,*) 'Invalid board size'
            stop
        end if

        return

    end subroutine

    subroutine movepiece(state, move)
        implicit none
        type(board) :: state
        type(act)  :: move
        integer(pi) :: player, x, y, ix, iy, ipiece, ip
        x = move%i
        y = move%j
        player = state%squares(x,y)
        if(player.eq.p1) then
            ip = 1
        else
            ip = 2
        end if

        state%turn = -state%turn
        state%squares(x,y) = empty
        select case(move%d)
            case('N')
                state%squares(x,y-1) = player
            case('E')
                state%squares(x+1,y) = player
            case('S')
                state%squares(x,y+1) = player
            case('W')
                state%squares(x-1,y) = player
        end select

        do ipiece = 1, 4
            ix = state%pieces(1,ipiece,ip)
            iy = state%pieces(2,ipiece,ip)
            if(ix.eq.x .and. iy.eq.y) then
                select case(move%d)
                    case('N')
                        state%pieces(2,ipiece,ip) = y - one
                    case('E')
                        state%pieces(1,ipiece,ip) = x + one
                    case('S')
                        state%pieces(2,ipiece,ip) = y + one
                    case('W')
                        state%pieces(1,ipiece,ip) = x - one
                end select
                exit
            end if
        end do

        return
    end subroutine

    subroutine printboard(state)
        implicit none
        type(board) :: state
        integer(pi) :: i, j
        CHARACTER(LEN=5) :: FMT
        CHARACTER(LEN=1) :: pb(imax, jmax)

        if(printt.eq.0) return
        write(FMT,'( "(", I1, "A2)" )') imax

        pb = ','
        do i = 1, imax
        do j = 1, jmax
            if(state%squares(i,j).eq.p1) pb(i,j) = '+'
            if(state%squares(i,j).eq.p2) pb(i,j) = '-'
        end do
        end do
        do j = 1, jmax
          write(*,*) pb(:,j)
        end do

        return
    end subroutine

    subroutine checkwinner(state)
        implicit none
        type(board) :: state
        integer(pi) :: i, j, p, pp
        integer(pi4) :: xi, yi, xj, yj, xk, yk, dx, dy

        state%winner = empty
        p = -state%turn
        if(p.eq.p1) pp = 1
        if(p.eq.p2) pp = 2

        do i = 1, 3
            xi = state%pieces(1,i,pp)
            yi = state%pieces(2,i,pp)
            do j = i+one, 4

                xj = state%pieces(1,j,pp)
                yj = state%pieces(2,j,pp)

                dx = xi - xj
                dy = yi - yj

                if(abs(dx).le.1 .and. abs(dy).le.1) then
                    ! Vertical
                    if(dx.eq.0) then
                        yk = max(yi, yj) + 1
                        if(yk.le.jmax) then
                            if(state%squares(xi, yk).eq.p) then
                                state%winner = p
                                return
                            end if
                        end if
                        yk = min(yi, yj) - 1
                        if(yk.ge.1) then
                            if(state%squares(xi, yk).eq.p) then
                                state%winner = p
                                return
                            end if
                        end if
                    end if

                    ! Horizontal
                    if(dy.eq.0) then
                        xk = max(xi, xj) + 1
                        if(xk.le.imax) then
                            if(state%squares(xk, yi).eq.p) then
                                state%winner = p
                                return
                            end if
                        end if
                        xk = min(xi, xj) - 1
                        if(xk.ge.1) then
                            if(state%squares(xk, yi).eq.p) then
                                state%winner = p
                                return
                            end if
                        end if
                    end if

                    ! Diagonal
                    if(dx*dy.eq.1) then
                        xk = max(xi, xj) + 1
                        yk = max(yi, yj) + 1
                        if(xk.le.imax .and. yk.le.jmax) then
                            if(state%squares(xk, yk).eq.p) then
                                state%winner = p
                                return
                            end if
                        end if
                        xk = min(xi, xj) - 1
                        yk = min(yi, yj) - 1
                        if(xk.ge.1 .and. yk.ge.1) then
                            if(state%squares(xk, yk).eq.p) then
                                state%winner = p
                                return
                            end if
                        end if
                    end if

                    ! Diagonal
                    if(dx*dy.eq.-1) then
                        xk = min(xi, xj) - 1
                        yk = max(yi, yj) + 1
                        if(xk.ge.1 .and. yk.le.jmax) then
                            if(state%squares(xk, yk).eq.p) then
                                state%winner = p
                                return
                            end if
                        end if
                        xk = max(xi, xj) + 1
                        yk = min(yi, yj) - 1
                        if(xk.le.imax .and. yk.ge.1) then
                            if(state%squares(xk, yk).eq.p) then
                                state%winner = p
                                return
                            end if
                        end if
                    end if

                end if

            end do
        end do

    end subroutine


    subroutine makechild(node, child, move)
        type(board) :: node, child
        type(act) :: move
!       child%squares = node%squares
!       child%pieces  = node%pieces
!       child%winner  = node%winner
!       child%depth   = node%depth + 1
!       child%turn    = -node%turn
        child = node
        child%depth = child%depth + 1
        call movepiece(child, move)
    end subroutine

end module boardclass
