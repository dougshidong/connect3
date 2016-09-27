module ai

    use prec
    use globals
    use boardclass
    implicit none

    contains

    recursive subroutine alphabeta( &
        v, node, ialpha, ibeta, iplayer, depth, imaxdepth, bestmove, htype)

        implicit none

        type(board)  :: node
        type(board), allocatable  :: rootchildren(:)
        integer(pi4), allocatable :: values(:)
        integer(pi)  :: iplayer, htype
        integer(pi4) :: ialpha, ibeta, imaxdepth
        integer(pi4) :: v, vab, depth, alpha, beta, maxdepth, vtemp
        integer(pi)  :: player, ichild, jchild, nchildren
        type(act)    :: acts(16), bestmove, tempmove
        real         :: mixit

        alpha = ialpha
        beta = ibeta
        player = iplayer
        maxdepth = imaxdepth

        nnode = nnode + 1

        v = 0
        vab = 0
        call checkwinner(node)
        if(node%winner.ne.0) then
!           Substracting node depth encourages the fewest winning moves
!           Without depth, it may 'taunt' the enemy with an uppper hand forever
            v = evalHeuristic(node, htype)
            v = v + node%winner * (big-500000)
            if(node%winner.eq.iam) then
                v = v - iam * (node%depth * 10000)
            else
                v = v + iam * (node%depth * 10000)
            end if
            return
        end if

        if(depth.eq.maxdepth) then
            v = evalHeuristic(node, htype)
            return
        end if

        if(depth.eq.0) then
          call init_random_seed()
        end if
!       Maximizing player
        if(player.eq.p1) then
            v = -big
            call generateActions(node, p1, nchildren, acts)

            ! Randomize actions such that moves with equal values are randomly ordered
            if(depth.eq.0) then
                do ichild = nchildren, 2, -1
                    call random_number(mixit)
                    jchild = 1 + floor(ichild*mixit)
                    tempmove = acts(jchild)
                    acts(jchild) = acts(ichild)
                    acts(ichild) = tempmove
                end do
            end if

            ! Run alpha-beta on all children up to a depth of (maxdepth - sortd)
            ! Then sort moves such that maxmimum moves occur first
            if(depth.eq.0 .and. sortd.ne.0) then

                allocate(rootchildren(nchildren))
                allocate(values(nchildren))
                do ichild = 1, nchildren
                    allocate(rootchildren(ichild)%squares(imax, jmax))
                    call makechild(node, rootchildren(ichild), acts(ichild))

                    vab = 0
                    call alphabeta(vab, rootchildren(ichild), &
                            alpha, beta, p2, depth + 1, maxdepth - sortd, bestmove, htype)
                    v = max(v, vab)
                    alpha = max(alpha, v)
                    values(ichild) = v
                    if(beta.le.alpha) exit
                end do
                do ichild = 1, nchildren
                    if(allocated(rootchildren(ichild)%squares)) then
                        deallocate(rootchildren(ichild)%squares)
                    end if
                end do
                deallocate(rootchildren)

                ! Bubble Sort
                do ichild = 1, nchildren
                do jchild = nchildren, ichild + 1, -1
                    if (values(jchild-1)<values(jchild)) then
                        vtemp = values(jchild-1)
                        values(jchild-1) = values(jchild)
                        values(jchild) = vtemp
                        tempmove = acts(jchild-1)
                        acts(jchild-1) = acts(jchild)
                        acts(jchild) = tempmove
                    end if
                end do
                end do

                deallocate(values)
                v = -big
                alpha = -big
                beta = big
            end if

            allocate(node%children(nchildren))
            do ichild = 1, nchildren
                allocate(node%children(ichild)%squares(imax, jmax))
                call makechild(node, node%children(ichild), acts(ichild))

                vab = 0
                call alphabeta(vab, node%children(ichild), &
                        alpha, beta, p2, depth + 1, maxdepth, bestmove, htype)

                if(depth.eq.0) then
                    if(vab.gt.v) then
                        bestmove = acts(ichild)
                    endif
                end if

                v = max(v, vab)

                alpha = max(alpha, v)

                if(depth.eq.0 .and. printt.eq.1) then
                    write(*,'(I2," out of ", I2, ", Move: ", 2I1,A1,", value:",I0," Best Move: ", 2I1,A1)') &
                    ichild, nchildren, acts(ichild)%i, acts(ichild)%j, acts(ichild)%d, v, bestmove%i, bestmove%j, bestmove%d
                end if

                if(beta.le.alpha) exit
            end do

            do ichild = 1, nchildren
                if(allocated(node%children(ichild)%squares)) then
                    deallocate(node%children(ichild)%squares)
                end if
            end do

            deallocate(node%children)

            return
!       Minimizing player
        else
            v = big
            call generateActions(node, p2, nchildren, acts)

            ! Randomize actions such that moves with equal values are randomly ordered
            if(depth.eq.0) then
                do ichild = nchildren, 2, -1
                    call random_number(mixit)
                    jchild = 1 + floor(ichild*mixit)
                    tempmove = acts(jchild)
                    acts(jchild) = acts(ichild)
                    acts(ichild) = tempmove
                end do
            end if

            ! Run alpha-beta on all children up to a depth of (maxdepth - sortd)
            ! Then sort moves such that minimum moves occur first
            if(depth.eq.0 .and. sortd.ne.0) then

                allocate(rootchildren(nchildren))
                allocate(values(nchildren))
                do ichild = 1, nchildren
                    allocate(rootchildren(ichild)%squares(imax, jmax))
                    call makechild(node, rootchildren(ichild), acts(ichild))

                    vab = 0
                    call alphabeta(vab, rootchildren(ichild), &
                            alpha, beta, p1, depth + 1, maxdepth - sortd, bestmove, htype)
                    v = min(v, vab)
                    beta = min(beta, v)
                    values(ichild) = v
                    if(beta.le.alpha) exit
                end do
                do ichild = 1, nchildren
                    if(allocated(rootchildren(ichild)%squares)) then
                        deallocate(rootchildren(ichild)%squares)
                    end if
                end do
                deallocate(rootchildren)

                ! Bubble Sort
                do ichild = 1, nchildren
                do jchild = nchildren, ichild + 1, -1
                    if (values(jchild-1)>values(jchild)) then
                        vtemp = values(jchild-1)
                        values(jchild-1) = values(jchild)
                        values(jchild) = vtemp
                        tempmove = acts(jchild-1)
                        acts(jchild-1) = acts(jchild)
                        acts(jchild) = tempmove
                    end if
                end do
                end do

                deallocate(values)
                v = big
                alpha = -big
                beta = big
            end if

            allocate(node%children(nchildren))
            do ichild = 1, nchildren
                allocate(node%children(ichild)%squares(imax, jmax))
                call makechild(node, node%children(ichild), acts(ichild))

                vab = 0
                call alphabeta(vab, node%children(ichild), &
                        alpha, beta, p1, depth + 1, maxdepth, bestmove, htype)
                if(depth.eq.0) then
                    if(vab.lt.v) then
                        bestmove = acts(ichild)
                    endif
                end if

                v = min(v, vab)

                beta = min(beta, v)

                if(depth.eq.0 .and. printt.eq.1) then
                    write(*,'(I2," out of ", I2, ", Move: ", 2I1,A1,", value:",I0," Best Move: ", 2I1,A1)') &
                    ichild, nchildren, acts(ichild)%i, acts(ichild)%j, acts(ichild)%d, v, bestmove%i, bestmove%j, bestmove%d
                end if

                if(beta.le.alpha) exit
            end do

            do ichild = 1, nchildren
                if(allocated(node%children(ichild)%squares)) then
                    deallocate(node%children(ichild)%squares)
                end if
            end do

            deallocate(node%children)

            return
        end if


        return

    end subroutine

    SUBROUTINE init_random_seed()
      INTEGER :: i, n, clock
      INTEGER, DIMENSION(:), ALLOCATABLE :: seed

      n = 1
      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))

      CALL SYSTEM_CLOCK(COUNT=clock)

      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      CALL RANDOM_SEED(PUT = seed)

      DEALLOCATE(seed)
    END SUBROUTINE

    function evalHeuristic(state, htype) result(h)
        implicit none
        type (board) :: state
        integer(pi4) :: h
        integer(pi)  :: htype

        select case(htype)
            case(0)
                h = h2run(state)
            case(1)
                h = hdist(state)
        end select

        return

    end function

    function h2run(state) result(h)
        implicit none
        type (board) :: state
        integer(pi4) :: h
        integer(pi)  :: i, j
        integer(pi)  :: xi, yi, xj, yj, dx, dy, xk, yk

        h = 0
        h = hdist(state)

        do i = 1, 3

            xi = state%pieces(1,i,1)
            yi = state%pieces(2,i,1)
            do j = i+one, 4
!               Player 1 White
                xj = state%pieces(1,j,1)
                yj = state%pieces(2,j,1)

                dx = xi - xj
                dy = yi - yj

                if(abs(dx).le.1 .and. abs(dy).le.1) then
                    h = h + 5

!                   if(dx.eq.0) then
!                       yk = max(yi, yj) + 1
!                       if(yk.le.jmax) then
!                           if(state%squares(xi, yk).eq.empty) then
!                               h = h + 1
!                           end if
!                       end if
!                       yk = min(yi, yj) - 1
!                       if(yk.ge.1) then
!                           if(state%squares(xi, yk).eq.empty) then
!                               h = h + 1
!                           end if
!                       end if
!                   end if

!                   if(dy.eq.0) then
!                       xk = max(xi, xj) + 1
!                       if(xk.le.imax) then
!                           if(state%squares(xk, yi).eq.empty) then
!                               h = h + 1
!                           end if
!                       end if
!                       xk = min(xi, xj) - 1
!                       if(xk.ge.1) then
!                           if(state%squares(xk, yi).eq.empty) then
!                               h = h + 1
!                           end if
!                       end if
!                   end if

!                   if(dx*dy.eq.1) then
!                       xk = max(xi, xj) + 1
!                       yk = max(yi, yj) + 1
!                       if(xk.le.imax .and. yk.le.jmax) then
!                           if(state%squares(xk, yk).eq.empty) then
!                               h = h + 1
!                           end if
!                       end if
!                       xk = min(xi, xj) - 1
!                       yk = min(yi, yj) - 1
!                       if(xk.ge.1 .and. yk.ge.1) then
!                           if(state%squares(xk, yk).eq.empty) then
!                               h = h + 1
!                           end if
!                       end if
!                   end if

!                   if(dx*dy.eq.-1) then
!                       xk = min(xi, xj) - 1
!                       yk = max(yi, yj) + 1
!                       if(xk.ge.1 .and. yk.le.jmax) then
!                           if(state%squares(xk, yk).eq.empty) then
!                               h = h + 1
!                           end if
!                       end if
!                       xk = max(xi, xj) + 1
!                       yk = min(yi, yj) - 1
!                       if(xk.le.imax .and. yk.ge.1) then
!                           if(state%squares(xk, yk).eq.empty) then
!                               h = h + 1
!                           end if
!                       end if
!                   end if

                end if
                ! End of P1 White H

!               Player 2 Black
                xj = state%pieces(1,j,2)
                yj = state%pieces(2,j,2)

                dx = xi - xj
                dy = yi - yj

                if(abs(dx).le.1 .and. abs(dy).le.1) then
                    h = h - 5

!                   if(dx.eq.0) then
!                       yk = max(yi, yj) + 1
!                       if(yk.le.jmax) then
!                           if(state%squares(xi, yk).eq.empty) then
!                               h = h - 1
!                           end if
!                       end if
!                       yk = min(yi, yj) - 1
!                       if(yk.ge.1) then
!                           if(state%squares(xi, yk).eq.empty) then
!                               h = h - 1
!                           end if
!                       end if
!                   end if

!                   if(dy.eq.0) then
!                       xk = max(xi, xj) + 1
!                       if(xk.le.imax) then
!                           if(state%squares(xk, yi).eq.empty) then
!                               h = h - 1
!                           end if
!                       end if
!                       xk = min(xi, xj) - 1
!                       if(xk.ge.1) then
!                           if(state%squares(xk, yi).eq.empty) then
!                               h = h - 1
!                           end if
!                       end if
!                   end if

!                   if(dx*dy.eq.1) then
!                       xk = max(xi, xj) + 1
!                       yk = max(yi, yj) + 1
!                       if(xk.le.imax .and. yk.le.jmax) then
!                           if(state%squares(xk, yk).eq.empty) then
!                               h = h - 1
!                           end if
!                       end if
!                       xk = min(xi, xj) - 1
!                       yk = min(yi, yj) - 1
!                       if(xk.ge.1 .and. yk.ge.1) then
!                           if(state%squares(xk, yk).eq.empty) then
!                               h = h - 1
!                           end if
!                       end if
!                   end if

!                   if(dx*dy.eq.-1) then
!                       xk = min(xi, xj) - 1
!                       yk = max(yi, yj) + 1
!                       if(xk.ge.1 .and. yk.le.jmax) then
!                           if(state%squares(xk, yk).eq.empty) then
!                               h = h - 1
!                           end if
!                       end if
!                       xk = max(xi, xj) + 1
!                       yk = min(yi, yj) - 1
!                       if(xk.le.imax .and. yk.ge.1) then
!                           if(state%squares(xk, yk).eq.empty) then
!                               h = h - 1
!                           end if
!                       end if
!                   end if

                end if
                ! End of P2 Black H

            end do
        end do

        do i = 1,  4
            xi = state%pieces(1,i,1)
            h = h - abs(xi-(imax+1)/2) ! Ideally, pieces go towards the center
            yi = state%pieces(2,i,1)
            h = h - floor(abs(yi-(jmax+1)/2.0)) ! Ideally, pieces go towards the center
            xi = state%pieces(1,i,2)
            h = h + abs(xi-(imax+1)) ! Ideally, pieces go towards the center
            yi = state%pieces(2,i,2)
            h = h + floor(abs(yi-(jmax+1)/2.0)) ! Ideally, pieces go towards the center
        end do

        return

    end function

    function hdist(state) result(h)
        implicit none
        type (board) :: state
        integer(pi4) :: h, d1, d2
        integer(pi)  :: i, j
        integer(pi)  :: xi, yi, xj, yj

        h = 0

        d1 = 0
        d2 = 0
        do i = 1, 3
        do j = i+one, 4
            xi = state%pieces(1,i,1)
            yi = state%pieces(2,i,1)

            xj = state%pieces(1,j,1)
            yj = state%pieces(2,j,1)

            d1 = (xi - xj)**2 + (yi - yj)**2
            h = h - d1

            xi = state%pieces(1,i,2)
            yi = state%pieces(2,i,2)

            xj = state%pieces(1,j,2)
            yj = state%pieces(2,j,2)

            d2 = (xi - xj)**2 + (yi - yj)**2

            h = h + d2
        end do
        end do

        return

    end function

    subroutine generateActions(state, player, nmove, moves)
    implicit none
    type(board) :: state
    integer(pi) :: player, ip, ipiece, x, y, p, nmove
    type(act)   :: moves(16)

    nmove = 0
    if(player.eq.p1) then
        ip = 1
    else
        ip = 2
    end if

    do ipiece = 1, 4
        x = state%pieces(1,ipiece,ip)
        y = state%pieces(2,ipiece,ip)

        if(x.ne.imax) then
            p = state%squares(x+1,y)
            if(p.eq.empty) then
                nmove = nmove + one
                moves(nmove) % i = x
                moves(nmove) % j = y
                moves(nmove) % d = 'E'
            end if
        end if
        if(y.ne.jmax) then
            p = state%squares(x,y+1)
            if(p.eq.empty) then
                nmove = nmove + one
                moves(nmove) % i = x
                moves(nmove) % j = y
                moves(nmove) % d = 'S'
            end if
        end if
        if(x.ne.1) then
            p = state%squares(x-1,y)
            if(p.eq.empty) then
                nmove = nmove + one
                moves(nmove) % i = x
                moves(nmove) % j = y
                moves(nmove) % d = 'W'
            end if
        end if
        if(y.ne.1) then
            p = state%squares(x,y-1)
            if(p.eq.empty) then
                nmove = nmove + one
                moves(nmove) % i = x
                moves(nmove) % j = y
                moves(nmove) % d = 'N'
            end if
        end if

    end do

    end subroutine


end module ai
