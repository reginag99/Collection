function [approx] = LinearClassNormalEqExample2(y,x,m)
    % ----------------------------------------
    %   Solution of least squares  problem  min_x || Ax - y ||_2
    %   using the method of normal equations.
    %   Matrix  A is constructed as a Vandermonde matrix.
    % ----------------------------------------


     d=5;  % degree of the polynomial
    %m=50;%number of discretization points or rows in the matrix A

    %y=zeros(1,m);
    A=[];

    %Values for parameter eta -------------------------------------------------
    scaling_factor = 0.7;
    %function_flag = 1;  % flag means choose of the model function for eta: can be chosen between 1 and 4
    %eta(t) = scaling_factor*function

    % ext_eta = ExactEta(scaling_factor,function_flag,x); %Exact eta.



    % construction of a Vandermonde matrix

    for i=1:m
      for j=1:d+1
        A(i,j)=power(x(i),j-1);
      end
    end


    % plot(x,y,'o')

    % computing the right hand side in the method of normal equations
    c=A'*y';

    % computing matrix in the left hand side in the method of normal equations
    C=A'*A;

    l=zeros(d+1);

    % solution of the normal equation using Cholesky decomposition

    for j=1:d+1
      s1=0;
      for k=1:j-1
        s1=s1+l(j,k)*l(j,k);
      end
      l(j,j)=(C(j,j)-s1)^(1/2);
      for i=j+1:d+1
        s2=0;
        for k=1:j-1
          s2=s2+l(i,k)*l(j,k);
        end
        l(i,j)=(C(i,j)-s2)/l(j,j);
      end
    end
    for i=1:d+1
      for k=1:1:i-1
        c(i)=c(i)-c(k)*l(i,k);
      end
      c(i)=c(i)/l(i,i);
    end
    for i=d+1:-1:1
      for k=d+1:-1:i+1
        c(i)=c(i)-c(k)*l(k,i);
      end
      c(i)=c(i)/l(i,i);
    end

    size(x);

    size(y);
    figure(1)
    plot(x,y,'o r', 'linewidth',2)
    hold on

    % compute approximation to this exact polynomial with comp. coefficients c

    approx = A*c;
    plot(x,approx,'*- ', 'linewidth',2)


    str_xlabel = ['poly.degree d=', num2str(d)];



    xlabel('Time')
    legend('\eta from measured data',' approximated guess for \eta_0');
    title(['Least squares  for classification, number of input points ',num2str(m)])

    % computation of the relative error as
    %  norm(approx. value - true value) / norm(true value)
    %e1=norm(y'- approx)/norm(y')
end