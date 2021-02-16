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
    class NodeMailItem : TreeNode
    {
        public NodeMailItem(Outlook.MailItem AMailItem)
        {
            this.MailItem = AMailItem;
        }
        private Outlook.MailItem mailItem;

        public Outlook.MailItem MailItem
        {
            get { return mailItem; }
            set
            {
                mailItem = value;
                if (mailItem != null)
                {
                    this.Text = mailItem.Subject;
                    this.Text = mailItem.Subject;
                }
            }
        }

    }
}
