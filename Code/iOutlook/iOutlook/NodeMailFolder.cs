using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.Office.Interop.Outlook;
using Outlook = Microsoft.Office.Interop.Outlook;

namespace iOutlook
{
    class NodeMailFolder : NodeItem
    {
        private Outlook.MAPIFolder folder;

        public Outlook.MAPIFolder Folder
        {
            get { return folder; }
            set
            {
                folder = value;
                if (folder != null)
                {
                    this.Text = folder.Name;
                }
            }
        }
        public NodeMailFolder(Outlook.MAPIFolder AFolder)
        {
            this.Folder = AFolder;
        }

        public NodeMailFolder LoadItems()
        {
            if (Nodes.Count == 0 && folder != null)
            {
                for (int i = 1; i <= folder.Folders.Count; i++)
                {
                    Nodes.Add(new NodeMailFolder(folder.Folders[i]));
                }
                for (int i = 1; i <= folder.Items.Count; i++)
                {
                    Nodes.Add(new NodeMailItem(folder.Items[i]));
                }
            }
            return this;
        }

    }
}
