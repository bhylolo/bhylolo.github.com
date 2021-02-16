using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.Office.Interop.Outlook;
using Outlook = Microsoft.Office.Interop.Outlook;

namespace iOutlook
{
    public partial class FormMain : Form
    {
        public FormMain()
        {
            InitializeComponent();
        }
        private Outlook.Application outlookapp;

        public Outlook.Application OutlookApp
        {
            get
            {
                if (outlookapp == null)
                {
                    outlookapp = new Outlook.Application();
                }
                return outlookapp;
            }
        }
        public bool GetRootFolders(string PSTFile, Action<Outlook.MAPIFolder> Getter)
        {
            Outlook.Folders folders = null;

            if (OutlookApp != null)
            {
                if (!string.IsNullOrEmpty(PSTFile))
                {
                    OutlookApp.Session.AddStore(PSTFile);
                }
                folders = OutlookApp.GetNamespace("MAPI").Folders;
                if (folders != null)
                {
                    for (int i = 1; i <= folders.Count; i++)
                    {
                        Getter(folders[i]);
                    }
                }
            }
            return folders != null;
        }
        private void toolStripButton1_Click(object sender, EventArgs e)
        {
            treeView1.Nodes.Clear();
            GetRootFolders("",
                (f) =>
                {
                    treeView1.Nodes.Add(new NodeMailFolder(f));
                }
                );
        }

        private void treeView1_NodeMouseDoubleClick(object sender, TreeNodeMouseClickEventArgs e)
        {
            if (e.Node is NodeMailFolder)
            {
                (e.Node as NodeMailFolder).LoadItems().Expand();
            }
        }
    }
}
