```
 ______   ______   ______    ___   ___       _______   ______   ___ __ __    _______
/_____/\ /_____/\ /_____/\  /___/\/__/\    /_______/\ /_____/\ /__//_//_/\ /_______/\
\::::_\/_\:::_ \ \\:::_ \ \ \::.\ \\ \ \   \::: _  \ \\:::_ \ \\::\| \| \ \\::: _  \ \
 \:\/___/\\:\ \ \ \\:(_) ) )_\:: \/_) \ \   \::(_)  \/_\:\ \ \ \\:.      \ \\::(_)  \/_
  \:::._\/ \:\ \ \ \\: __ `\ \\:. __  ( (    \::  _  \ \\:\ \ \ \\:.\-/\  \ \\::  _  \ \
   \:\ \    \:\_\ \ \\ \ `\ \ \\: \ )  \ \    \::(_)  \ \\:\_\ \ \\. \  \  \ \\::(_)  \ \
    \_\/     \_____\/ \_\/ \_\/ \__\/\__\/     \_______\/ \_____\/ \__\/ \__\/ \_______\/

```

Welcome, brave coder, to the mystical land of Bashlandia! Your quest is to aid the villagers by crafting powerful scripts to solve their everyday problems. Along the way, you'll encounter various challenges that will test your scripting skills. Ready your terminal, for your adventure begins now!

---

### Chapter 1: Understanding Fork Bombs

Your journey begins in the village of Safeville. The villagers need to understand what a fork bomb is and how it can impact their systems. Your first task is to explain what a fork bomb is and demonstrate it in a controlled environment.

**Fork Bomb Explanation:**
A fork bomb is a type of denial-of-service attack where a process continually replicates itself to deplete available system resources, potentially crashing the system.

**Controlled Fork Bomb Script:**

```bash
# Fork Bomb Script (Controlled)
# Note: This script is for educational purposes only. Running this on your system can crash it.
# Do not run this without proper safety measures.

:(){
  :|:&
};:
```

---

### Chapter 2: The Controlled Environment

In the village of Controltown, the villagers need to test the fork bomb without crashing their systems. Your task is to set up a controlled environment using VirtualBox or similar virtualization software to safely test the fork bomb.

**Setting Up a Controlled Environment:**

1. **Using VirtualBox:**

   - **Install VirtualBox:**
     - Download and install VirtualBox from the [official website](https://www.virtualbox.org/).
   - **Create a New Virtual Machine:**
     - Create a new VM with a Linux distribution (e.g., Ubuntu).
     - Allocate resources such as memory and CPU, making sure not to allocate too many resources to avoid crashing your host system.
   - **Snapshot the VM:**
     - Before running the fork bomb, take a snapshot of the VM. This allows you to easily revert to a safe state if the system crashes.
     - To create a snapshot, go to the VirtualBox Manager, select your VM, and click on the "Snapshots" tab, then click "Take Snapshot."

2. **Using VMWare:**
   - **Install VMWare:**
     - Download and install VMWare from the [official website](https://www.vmware.com/).
   - **Create a New Virtual Machine:**
     - Follow similar steps as for VirtualBox to set up a new VM with a Linux distribution.
   - **Snapshot the VM:**
     - Take a snapshot before running the fork bomb for easy recovery.

---

### Chapter 3: Limiting Resources

In the heart of Controltown lies the knowledge to limit system resources to prevent crashes. Write a script that limits the number of processes a user can create, thereby mitigating the impact of a fork bomb.

**Limiting Processes Script:**

```bash
# Limiting User Processes
echo "Enter the username to limit processes for:"
read username

echo "Enter the maximum number of processes:"
read max_procs

echo "$username hard nproc $max_procs" | sudo tee -a /etc/security/limits.conf
```

---

### Chapter 4: Monitoring System Resources

Your adventure leads you to the village of Monitoria, where the villagers need help monitoring their system resources. Write a script that monitors CPU and memory usage to detect potential fork bomb attacks.

**System Resource Monitoring Script:**

```bash
# System Resource Monitoring Script
while true; do
  echo "CPU Usage:"
  mpstat

  echo "Memory Usage:"
  free -h

  sleep 5
done
```

---

### Chapter 5: Creating Alerts

In the village of Alerton, the villagers need to be alerted when system resources are critically low. Write a script that sends an alert when CPU or memory usage exceeds a specified threshold.

**Alert Script:**

```bash
# Alert Script
threshold_cpu=80
threshold_mem=80

while true; do
  cpu_usage=$(mpstat | awk '$12 ~ /[0-9.]+/ { print 100 - $12 }')
  mem_usage=$(free | awk '/Mem:/ { printf("%.2f"), $3/$2*100 }')

  if (( ${cpu_usage%.*} > threshold_cpu )); then
    echo "Warning: CPU usage is above $threshold_cpu%"
  fi

  if (( ${mem_usage%.*} > threshold_mem )); then
    echo "Warning: Memory usage is above $threshold_mem%"
  fi

  sleep 5
done
```

---

### Chapter 6: Safe Termination

Further in your journey, you arrive at the town of Safetia, where the villagers require your skills to safely terminate runaway processes. Write a script that identifies and safely terminates processes that consume excessive resources.

**Safe Termination Script:**

```bash
# Safe Termination Script
echo "Enter the maximum CPU usage threshold:"
read cpu_threshold

echo "Enter the maximum memory usage threshold:"
read mem_threshold

while true; do
  ps -eo pid,ppid,cmd,%mem,%cpu --sort=-%mem | awk -v cpu=$cpu_threshold -v mem=$mem_threshold '
  $5 > cpu || $4 > mem { print "Killing process " $1 " (" $3 ")"; system("kill -9 " $1) }
  '
  sleep 5
done
```

---

### Chapter 7: Educating the Villagers

Your final challenge leads you to the Academy of Bashlandia in the village of Educatia. The villagers need to understand best practices to prevent and handle fork bombs. Write a guide that explains these best practices.

**Fork Bomb Prevention Guide:**

1. **Limit User Processes:**

   - Set limits on the number of processes a user can create using `/etc/security/limits.conf`.

2. **Monitor System Resources:**

   - Regularly monitor CPU and memory usage to detect unusual activity.

3. **Set Up Alerts:**

   - Create alerts to notify administrators when resource usage exceeds safe thresholds.

4. **Safe Termination:**

   - Implement scripts to safely terminate runaway processes that exceed resource limits.

5. **Use Controlled Environments:**
   - Test potentially harmful scripts in virtual machines or containers to avoid impacting the main system.

---

### Conclusion

Congratulations, valiant coder! You have successfully navigated the challenges of Bashlandia and helped the villagers understand and prevent fork bombs. Your journey has enhanced your skills, preparing you for even greater adventures ahead. May your terminal always be powerful and your scripts ever efficient!
