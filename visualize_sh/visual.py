import os

global_node = int(0)


def recursive_visulize(string : str, wsd_file, depth : int):
    global global_node
    tmp_root_node = global_node
    lp_index = string.find('(')
    rp_index = string.find(')')
    if(string.__len__ == 0):
        return string
    if((rp_index < lp_index) or (lp_index == -1)) and (rp_index != -1):
        if(rp_index != 0):
            wsd_file.write(str(global_node) + " : " + string[0:rp_index] + "\n")
        ret = string[rp_index + 1 : ]
        return ret
        
    tmp_str = string
    while (lp_index < rp_index) and (lp_index != -1) :
        if(depth == 1):
            wsd_file.write(str(global_node) + " : " + "Decl" + "\n")
        else:
            wsd_file.write(str(global_node) + " : " + tmp_str[0:lp_index] + "\n")
        global_node = global_node + 1
        wsd_file.write(str(tmp_root_node) + " --> " + str(global_node) + "\n")
        tmp_tmp_str = tmp_str[lp_index + 1:]
        tmp_str = recursive_visulize(tmp_tmp_str, wsd_file, depth + 1)
        lp_index = tmp_str.find('(')
        rp_index = tmp_str.find(')')        
    
    if((rp_index < lp_index) or (lp_index == -1)) and (rp_index != -1):
        tmp_str = tmp_str[rp_index + 1: ]
    return tmp_str

    
def visualize(string : str):
    wsd_file = open("./rust.wsd", "w+", encoding='utf-8')
    wsd_file.write("@startuml AST\n")
    first_line = str(global_node) + " : Module\n"
    
    wsd_file.write(first_line)
    
    recursive_visulize(string, wsd_file, 0)
    
    last_line = "state 0<<fork>> \n@enduml"
    wsd_file.write(last_line)
    wsd_file.close()
    

def main():
    # print(os.listdir())

    file = open("./out.txt", "r", encoding='utf-8')
    lines = file.readlines()
    file.close()

    visualize(lines[3])
    # print(lines[3])
    
if __name__ == "__main__":
    main()