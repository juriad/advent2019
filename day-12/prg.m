classdef prg
	methods(Static)
		function [pos, vel] = readPlanets(fileName)
			fileID = fopen(fileName, 'r');
			pos = fscanf(fileID, '<x=%d, y=%d, z=%d>\n', [3, Inf]);
			vel = zeros(size(pos));
			fclose(fileID);
		end

		function [pos, vel] = step(pos, vel, mn)
      d = sign(pos(:, mn(:, 1)) - pos(:, mn(:, 2)));
			for a = 1:size(mn, 1)
        vel(:, mn(a, 1)) -= d(:, a);
        vel(:, mn(a, 2)) += d(:, a);
			end
      pos = pos + vel;
		end
    
    function mn = makeMn(pos)
      mn = [];
      for m = 1:size(pos, 2)
        for n = (m + 1):size(pos, 2)
          mn = [mn; [m, n]];
        end
      end
    end

		function en = task1(pos, vel, n)
      mn = prg.makeMn(pos);
      
			for a = 1:n
				[pos, vel] = prg.step(pos, vel, mn);
			end
      en = sum(sum(abs(vel)) .* sum(abs(pos)));
		end
    
    function rep = task2(pos, vel)
      mn = prg.makeMn(pos);
      
      pos2 = pos;
			vel2 = vel;
			res = zeros(1, 4);
      
      ss = ones(1, 3);
      a = 1;
      
			while true
				[pos2, vel2] = prg.step(pos2, vel2, mn);
        s = sign(sum(abs(pos - pos2)'));
        if sum(s) < 3
          res = [res; [a, s]];
          ss = ss .* s;
          
          if sum(ss) == 0
            break
          endif
        endif
        
        if mod(a, 1000) == 0
          a
        endif
        a++;
			end
      
      a = diff(res(res(:, 2) == 0, :));
      b = diff(res(res(:, 3) == 0, :));
      c = diff(res(res(:, 4) == 0, :));

      rep = lcm(lcm(a(1, 1) + 1, b(1, 1) + 1), c(1, 1) + 1);
    end

		function [en, rep] = tasks(fileName, n)
			[pos, vel] = prg.readPlanets(fileName);
			en = prg.task1(pos, vel, n);
      rep = prg.task2(pos, vel);
		end
	end
end
