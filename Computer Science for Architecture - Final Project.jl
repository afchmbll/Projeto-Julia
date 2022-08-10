using Khepri
backend(autocad)

#Creates the shape's points
pontos_forma(p,r0,n)=
    let
        r2 = 2*sin(pi/3)*r0+r0
        p1=p+vpol(r0,0)
        p2=p+vpol(r0,2pi/3)
        p3=p+vpol(r0,4pi/3)
        arr = [p1+vpol(r0,i) for i in division(-pi/6,pi/6,n/12,false)]
        arr2 = [p3+vpol(r2,i) for i in division(pi/6, 3*pi/6,n*3/12,false)]
        arr3 = [p2+vpol(r0,i) for i in division(3*pi/6,5*pi/6,n/12,false)]
        arr4 = [p1+vpol(r2,i) for i in division(5*pi/6, 7*pi/6,n*3/12,false)]
        arr5 = [p3+vpol(r0,i) for i in division(7*pi/6, 9*pi/6,n/12,false)]
        arr6 = [p2+vpol(r2,i) for i in division(9*pi/6, 11*pi/6, n*3/12)]
        return [arr...,arr2...,arr3...,arr4...,arr5...,arr6...]
    end

#Creates a shape from the previously made points
forma(p,r0,n)=
    closed_spline(pontos_forma(p,r0,n))


#Extrudes the shape
laje(p,r,pts,h)=
    extrusion(surface(forma(p,r,pts)),vz(h))

#Creates the curve the building will have
curva(a,frequencia, x) =
    a*sin(frequencia*x)

#Receives (beyond the values of the last functions) the h value (height of slab), hp (height of floor), lvls(number of floors), base and abase (the same as 'lvls' and 'a', but to be used as auxiliary variable), 
#produces an array of shapes
sup_edificio(p,r,pts,h,hp,lvls,base,abase)=
    if lvls==0
        []
    else
        [forma(p,r,pts), 
        forma(p+vz(h),r,pts),
        sup_edificio(p+vz(hp),abase + curva(r/5,2*pi/(base*1.8*(h+hp)),(base*(h+hp)-lvls*(h+hp))),pts,h,hp,lvls-1,base,abase)...]
    end

#Creates the slabs for the building  
lajes_edificio(p,r,pts,h,hp,lvls,base,abase)=
    if lvls<5
        nothing
    else
        laje(p,r,pts,h)
        lajes_edificio(p+vz(hp),abase+curva(r/5,2*pi/(base*1.8*(h+hp)),(base*(h+hp)-lvls*(h+hp))),pts,h,hp,lvls-1,base,abase)
    end

#Creates an inner array of points
arrPontos(p,r0,n,lvls,h,hp,base,abase)=
    if lvls==0
        []
    else
        [pontos_forma(p+vz(h/2),0.1+r0,2*n), 
        arrPontos(p+vz(hp),abase+curva(r0/5,2*pi/(base*1.8*(h+hp)),(base*(h+hp)-lvls*(h+hp))),n,lvls-1,h,hp,base,abase)...]
    end

#Creates an outer array of points
arrPontos2(p,r0,n,lvls,h,hp,base,abase)=
    if lvls==0
        []
    else
        [pontos_forma(p+vz(h/2),1+r0,2*n), 
        arrPontos2(p+vz(hp),abase+curva(r0/5,2*pi/(base*1.8*(h+hp)),(base*(h+hp)-lvls*(h+hp))),n,lvls-1,h,hp,base,abase)...]
    end


#Creates the 1st zigzags 
linha1(arr1,arr2)=
    let
        for (i,j) in zip(arr1[1:4:end-1],arr2[3:4:end])
            cylinder(i,0.2,j)
        end
        for (i,j) in zip(arr2[3:4:end],arr1[5:4:end])
            cylinder(i,0.2,j) 
        end
    end

#Creates the reverse zigzags
linha2(arr1,arr2)=
    let
        for (i,j) in zip(arr1[3:4:end-1],arr2[5:4:end])
            cylinder(i,0.2,j)
        end
        for (i,j) in zip(arr2[1:4:end],arr1[3:4:end])
            cylinder(i,0.2,j) 
        end
    end

#Creates lines that start on top of the zigzags made in linha1()
linha3(arr1,arr2)=
        for (i,j) in zip(arr1[3:4:end],arr2[3:4:end])
            cylinder(i,0.2,j) 
        end


#Creates lines that start on top of the zigzags made in linha2()
linha4(arr1,arr2)=
        for (i,j) in zip(arr1[1:4:end],arr2[1:4:end])
            cylinder(i,0.2,j)
        end 

#Creates the cylinders that support the 'star' pannels
linha5(p,r,arr1,arr2,lvls,base)=
    if (base-lvls)%2!=0
        for (i,j) in zip(arr1[1:2:end-1],arr2[1:2:end-1])
            if distance(p,j)>r*1.5
                cylinder(i,0.075,j)
            end
        end
    else
        for (i,j) in zip(arr1[2:2:end-1],arr2[2:2:end-1])
            if distance(p,j)>r*1.5
                cylinder(i,0.075,j)
            end
        end
    end

#Creates cylinders all along the surface
cilindros(p,r,arr,arr2,lvls,hp,base)=
        if lvls<4
            nothing
        else
            linha5(p,r,arr[base-lvls],arr2[base-lvls],lvls,base)
            cilindros(p+vxz(curva(r/5,2*pi/(base*1.8*(p.x)),pi),hp),r,arr,arr2,lvls-1,hp,base)
        end


#Makes closed 'star' panels
estrelas_fechadas(p,r,arr1,arr2)=
    begin
        for (i,j,k) in zip(arr1[1:2:end],arr2[2:2:end],arr1[3:2:end])
            if distance(p,k)>r*1.5
                extrusion(surface(line(i,j,k,i)),0.1)
            end
        end
        for (i,j,k) in zip(arr2[2:2:end],arr1[3:2:end],arr2[4:2:end])
            if distance(p,k)>r*1.5
                extrusion(surface(line(i,j,k,i)),0.1)
            end
        end
    end



#Makes open 'star' panels      
estrelas_abertas(p,r,arr1,arr2)=
        begin
         for (i,j,k,l,m) in zip(arr1[1:2:end],arr2[2:2:end],arr1[3:2:end],arr2[1:2:end],arr2[3:2:end])
            quad_center(i,k,l,m)
            if distance(p,m)>r*1.5
                cone(quad_center(i,k,l,m),0.3,i)
                cone(quad_center(i,k,l,m),0.3,j)
                cone(quad_center(i,k,l,m),0.3,k)
            end
         end
         for (i,j,k,l,m) in zip(arr2[2:2:end],arr1[3:2:end],arr2[4:2:end],arr1[2:2:end],arr1[4:2:end])
            quad_center(i,k,l,m)
            if distance(p,m)>r*1.5
                cone(quad_center(i,k,l,m),0.3,i)
                cone(quad_center(i,k,l,m),0.3,j)
                cone(quad_center(i,k,l,m),0.3,k)
            end
         end
        end




#Creates the hexagons on the outside of the building
malha(arr,lvls,base)=
    let 
        if lvls>14
            linha1(arr[base-lvls],arr[base-lvls+2])
            lvls -= 2
            linha3(arr[base-lvls],arr[base-lvls+4])
            lvls -= 4
            linha2(arr[base-lvls],arr[base-lvls+2])
            lvls-= 2
            linha4(arr[base-lvls],arr[base-lvls+4])
            lvls-= 4
            malha(arr,lvls,base)
        else
            if lvls<5
                []
            elseif lvls<=14 && lvls>=5
                linha1(arr[base-lvls],arr[base-lvls+2])
                lvls-=2
                linha3(arr[base-lvls],arr[base-1])
            end
        end
    end



#Creates the panels along the surface
estrelas(p,r,arr,lvls,hp,base,aberto)=
     if lvls<5
        nothing
     elseif aberto != true && aberto != false
        println("Parâmetro 'aberto' tem de ser booleano")
        nothing
     elseif aberto==false
        estrelas_fechadas(p,r,arr[base-lvls],arr[base-lvls+1])
        estrelas_fechadas(p+vxz(curva(r/5,2*pi/(base*1.8*(p.x)),pi),hp),r,arr[base-lvls+2],arr[base-lvls+1])
        estrelas(p+vxz(2*curva(r/5,2*pi/(base*1.8*(p.x)),pi),2*hp),r,arr,lvls-2,hp,base,aberto)
     else
        estrelas_abertas(p,r,arr[base-lvls],arr[base-lvls+1])
        estrelas_abertas(p+vxz(curva(r/5,2*pi/(base*1.8*(p.x)),pi),hp),r,arr[base-lvls+2],arr[base-lvls+1])
        estrelas(p+vxz(2*curva(r/5,2*pi/(base*1.8*(p.x)),pi),2*hp),r,arr,lvls-2,hp,base,aberto)
     end
     
        

#Creates the building's surface from an array of shapes, slicing it on a specific point, and creating the hexagons and panels
edificio_final(p,r,pts,h,hp,lvls,aberto)=
    if pts%12!=0 || pts%2 != 0 || pts<=12
        println("ERRO - n tem de ser múltiplo de 12 e par")
        nothing
    else
        let base = lvls
            r0=r
            e=1
            n=pts
            abase=r

            shape = thicken(loft_ruled(sup_edificio(p,r,pts,h,hp,lvls,base,abase)),0.3)
            lajes_edificio(p,r,pts,h,hp,lvls,base,abase)
            slice(shape, 
               p+vz((lvls-2)*(hp)),
               vsph(1,0,atan((hp+h)/(r*2))))

            arrArr = arrPontos(p,r,pts,lvls,h,hp,base,abase)
            arrArr2 = arrPontos2(p,r,pts,lvls,h,hp,base,abase)

            malha(arrArr,lvls,base+1)
            cilindros(p+vpol(r*2,pi),r,arrArr2,arrArr,lvls,hp,base+1)           
            estrelas(p+vpol(r*2,pi),r,arrArr2,lvls,hp,base+1,aberto)
        end
    end


#Tests
delete_all_shapes()
edificio_final(x(24),5,24,0.4,3,28,false)
edificio_final(xy(60,30),5,24,0.4,3,28,true)


