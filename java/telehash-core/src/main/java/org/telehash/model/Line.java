/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model;

import java.net.InetSocketAddress;

import org.apache.mina.core.session.IoSession;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.telehash.Hash;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Line</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.telehash.model.Line#getAddress <em>Address</em>}</li>
 *   <li>{@link org.telehash.model.Line#getEnd <em>End</em>}</li>
 *   <li>{@link org.telehash.model.Line#getNeighbors <em>Neighbors</em>}</li>
 *   <li>{@link org.telehash.model.Line#getRingIn <em>Ring In</em>}</li>
 *   <li>{@link org.telehash.model.Line#getRingOut <em>Ring Out</em>}</li>
 *   <li>{@link org.telehash.model.Line#getInit <em>Init</em>}</li>
 *   <li>{@link org.telehash.model.Line#getSeenAt <em>Seen At</em>}</li>
 *   <li>{@link org.telehash.model.Line#getSentAt <em>Sent At</em>}</li>
 *   <li>{@link org.telehash.model.Line#getLineAt <em>Line At</em>}</li>
 *   <li>{@link org.telehash.model.Line#getTapLastAt <em>Tap Last At</em>}</li>
 *   <li>{@link org.telehash.model.Line#getBr <em>Br</em>}</li>
 *   <li>{@link org.telehash.model.Line#getBrIn <em>Br In</em>}</li>
 *   <li>{@link org.telehash.model.Line#getBrOut <em>Br Out</em>}</li>
 *   <li>{@link org.telehash.model.Line#getBsent <em>Bsent</em>}</li>
 *   <li>{@link org.telehash.model.Line#getLineId <em>Line Id</em>}</li>
 *   <li>{@link org.telehash.model.Line#isVisible <em>Visible</em>}</li>
 *   <li>{@link org.telehash.model.Line#isAdvertised <em>Advertised</em>}</li>
 *   <li>{@link org.telehash.model.Line#getRules <em>Rules</em>}</li>
 *   <li>{@link org.telehash.model.Line#getSession <em>Session</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.telehash.model.TelehashPackage#getLine()
 * @model
 * @generated
 */
public interface Line extends EObject {
	/**
	 * Returns the value of the '<em><b>Address</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Address</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Address</em>' attribute.
	 * @see #setAddress(InetSocketAddress)
	 * @see org.telehash.model.TelehashPackage#getLine_Address()
	 * @model dataType="org.telehash.model.Endpoint"
	 * @generated
	 */
	InetSocketAddress getAddress();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getAddress <em>Address</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Address</em>' attribute.
	 * @see #getAddress()
	 * @generated
	 */
	Line withAddress(InetSocketAddress value);

	void setAddress(InetSocketAddress value);

	/**
	 * Returns the value of the '<em><b>End</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>End</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>End</em>' attribute.
	 * @see #setEnd(Hash)
	 * @see org.telehash.model.TelehashPackage#getLine_End()
	 * @model dataType="org.telehash.model.Hash"
	 * @generated
	 */
	Hash getEnd();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getEnd <em>End</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>End</em>' attribute.
	 * @see #getEnd()
	 * @generated
	 */
	Line withEnd(Hash value);

	void setEnd(Hash value);

	/**
	 * Returns the value of the '<em><b>Neighbors</b></em>' attribute list.
	 * The list contents are of type {@link org.telehash.Hash}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Neighbors</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Neighbors</em>' attribute list.
	 * @see org.telehash.model.TelehashPackage#getLine_Neighbors()
	 * @model dataType="org.telehash.model.Hash"
	 * @generated
	 */
	EList<Hash> getNeighbors();

	/**
	 * Returns the value of the '<em><b>Ring In</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ring In</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ring In</em>' attribute.
	 * @see #isSetRingIn()
	 * @see #unsetRingIn()
	 * @see #setRingIn(int)
	 * @see org.telehash.model.TelehashPackage#getLine_RingIn()
	 * @model unsettable="true"
	 * @generated
	 */
	int getRingIn();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getRingIn <em>Ring In</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ring In</em>' attribute.
	 * @see #isSetRingIn()
	 * @see #unsetRingIn()
	 * @see #getRingIn()
	 * @generated
	 */
	Line withRingIn(int value);

	void setRingIn(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getRingIn <em>Ring In</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRingIn()
	 * @see #getRingIn()
	 * @see #setRingIn(int)
	 * @generated
	 */
	void unsetRingIn();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getRingIn <em>Ring In</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Ring In</em>' attribute is set.
	 * @see #unsetRingIn()
	 * @see #getRingIn()
	 * @see #setRingIn(int)
	 * @generated
	 */
	boolean isSetRingIn();

	/**
	 * Returns the value of the '<em><b>Ring Out</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ring Out</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ring Out</em>' attribute.
	 * @see #isSetRingOut()
	 * @see #unsetRingOut()
	 * @see #setRingOut(int)
	 * @see org.telehash.model.TelehashPackage#getLine_RingOut()
	 * @model unsettable="true"
	 * @generated
	 */
	int getRingOut();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getRingOut <em>Ring Out</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ring Out</em>' attribute.
	 * @see #isSetRingOut()
	 * @see #unsetRingOut()
	 * @see #getRingOut()
	 * @generated
	 */
	Line withRingOut(int value);

	void setRingOut(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getRingOut <em>Ring Out</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRingOut()
	 * @see #getRingOut()
	 * @see #setRingOut(int)
	 * @generated
	 */
	void unsetRingOut();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getRingOut <em>Ring Out</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Ring Out</em>' attribute is set.
	 * @see #unsetRingOut()
	 * @see #getRingOut()
	 * @see #setRingOut(int)
	 * @generated
	 */
	boolean isSetRingOut();

	/**
	 * Returns the value of the '<em><b>Init</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Init</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Init</em>' attribute.
	 * @see #isSetInit()
	 * @see #unsetInit()
	 * @see #setInit(long)
	 * @see org.telehash.model.TelehashPackage#getLine_Init()
	 * @model unsettable="true"
	 * @generated
	 */
	long getInit();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getInit <em>Init</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Init</em>' attribute.
	 * @see #isSetInit()
	 * @see #unsetInit()
	 * @see #getInit()
	 * @generated
	 */
	Line withInit(long value);

	void setInit(long value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getInit <em>Init</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetInit()
	 * @see #getInit()
	 * @see #setInit(long)
	 * @generated
	 */
	void unsetInit();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getInit <em>Init</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Init</em>' attribute is set.
	 * @see #unsetInit()
	 * @see #getInit()
	 * @see #setInit(long)
	 * @generated
	 */
	boolean isSetInit();

	/**
	 * Returns the value of the '<em><b>Seen At</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Seen At</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Seen At</em>' attribute.
	 * @see #isSetSeenAt()
	 * @see #unsetSeenAt()
	 * @see #setSeenAt(long)
	 * @see org.telehash.model.TelehashPackage#getLine_SeenAt()
	 * @model unsettable="true"
	 * @generated
	 */
	long getSeenAt();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getSeenAt <em>Seen At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Seen At</em>' attribute.
	 * @see #isSetSeenAt()
	 * @see #unsetSeenAt()
	 * @see #getSeenAt()
	 * @generated
	 */
	Line withSeenAt(long value);

	void setSeenAt(long value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getSeenAt <em>Seen At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetSeenAt()
	 * @see #getSeenAt()
	 * @see #setSeenAt(long)
	 * @generated
	 */
	void unsetSeenAt();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getSeenAt <em>Seen At</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Seen At</em>' attribute is set.
	 * @see #unsetSeenAt()
	 * @see #getSeenAt()
	 * @see #setSeenAt(long)
	 * @generated
	 */
	boolean isSetSeenAt();

	/**
	 * Returns the value of the '<em><b>Sent At</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Sent At</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Sent At</em>' attribute.
	 * @see #isSetSentAt()
	 * @see #unsetSentAt()
	 * @see #setSentAt(long)
	 * @see org.telehash.model.TelehashPackage#getLine_SentAt()
	 * @model unsettable="true"
	 * @generated
	 */
	long getSentAt();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getSentAt <em>Sent At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Sent At</em>' attribute.
	 * @see #isSetSentAt()
	 * @see #unsetSentAt()
	 * @see #getSentAt()
	 * @generated
	 */
	Line withSentAt(long value);

	void setSentAt(long value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getSentAt <em>Sent At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetSentAt()
	 * @see #getSentAt()
	 * @see #setSentAt(long)
	 * @generated
	 */
	void unsetSentAt();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getSentAt <em>Sent At</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Sent At</em>' attribute is set.
	 * @see #unsetSentAt()
	 * @see #getSentAt()
	 * @see #setSentAt(long)
	 * @generated
	 */
	boolean isSetSentAt();

	/**
	 * Returns the value of the '<em><b>Line At</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Line At</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Line At</em>' attribute.
	 * @see #isSetLineAt()
	 * @see #unsetLineAt()
	 * @see #setLineAt(long)
	 * @see org.telehash.model.TelehashPackage#getLine_LineAt()
	 * @model unsettable="true"
	 * @generated
	 */
	long getLineAt();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getLineAt <em>Line At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Line At</em>' attribute.
	 * @see #isSetLineAt()
	 * @see #unsetLineAt()
	 * @see #getLineAt()
	 * @generated
	 */
	Line withLineAt(long value);

	void setLineAt(long value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getLineAt <em>Line At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetLineAt()
	 * @see #getLineAt()
	 * @see #setLineAt(long)
	 * @generated
	 */
	void unsetLineAt();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getLineAt <em>Line At</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Line At</em>' attribute is set.
	 * @see #unsetLineAt()
	 * @see #getLineAt()
	 * @see #setLineAt(long)
	 * @generated
	 */
	boolean isSetLineAt();

	/**
	 * Returns the value of the '<em><b>Tap Last At</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Tap Last At</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Tap Last At</em>' attribute.
	 * @see #isSetTapLastAt()
	 * @see #unsetTapLastAt()
	 * @see #setTapLastAt(int)
	 * @see org.telehash.model.TelehashPackage#getLine_TapLastAt()
	 * @model unsettable="true"
	 * @generated
	 */
	int getTapLastAt();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getTapLastAt <em>Tap Last At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Tap Last At</em>' attribute.
	 * @see #isSetTapLastAt()
	 * @see #unsetTapLastAt()
	 * @see #getTapLastAt()
	 * @generated
	 */
	Line withTapLastAt(int value);

	void setTapLastAt(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getTapLastAt <em>Tap Last At</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetTapLastAt()
	 * @see #getTapLastAt()
	 * @see #setTapLastAt(int)
	 * @generated
	 */
	void unsetTapLastAt();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getTapLastAt <em>Tap Last At</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Tap Last At</em>' attribute is set.
	 * @see #unsetTapLastAt()
	 * @see #getTapLastAt()
	 * @see #setTapLastAt(int)
	 * @generated
	 */
	boolean isSetTapLastAt();

	/**
	 * Returns the value of the '<em><b>Br</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Br</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Br</em>' attribute.
	 * @see #setBr(int)
	 * @see org.telehash.model.TelehashPackage#getLine_Br()
	 * @model default="0"
	 * @generated
	 */
	int getBr();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getBr <em>Br</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Br</em>' attribute.
	 * @see #getBr()
	 * @generated
	 */
	Line withBr(int value);

	void setBr(int value);

	/**
	 * Returns the value of the '<em><b>Br In</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Br In</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Br In</em>' attribute.
	 * @see #setBrIn(int)
	 * @see org.telehash.model.TelehashPackage#getLine_BrIn()
	 * @model default="0"
	 * @generated
	 */
	int getBrIn();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getBrIn <em>Br In</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Br In</em>' attribute.
	 * @see #getBrIn()
	 * @generated
	 */
	Line withBrIn(int value);

	void setBrIn(int value);

	/**
	 * Returns the value of the '<em><b>Br Out</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Br Out</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Br Out</em>' attribute.
	 * @see #setBrOut(int)
	 * @see org.telehash.model.TelehashPackage#getLine_BrOut()
	 * @model default="0"
	 * @generated
	 */
	int getBrOut();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getBrOut <em>Br Out</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Br Out</em>' attribute.
	 * @see #getBrOut()
	 * @generated
	 */
	Line withBrOut(int value);

	void setBrOut(int value);

	/**
	 * Returns the value of the '<em><b>Bsent</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Bsent</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Bsent</em>' attribute.
	 * @see #setBsent(int)
	 * @see org.telehash.model.TelehashPackage#getLine_Bsent()
	 * @model default="0"
	 * @generated
	 */
	int getBsent();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getBsent <em>Bsent</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Bsent</em>' attribute.
	 * @see #getBsent()
	 * @generated
	 */
	Line withBsent(int value);

	void setBsent(int value);

	/**
	 * Returns the value of the '<em><b>Line Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Line Id</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Line Id</em>' attribute.
	 * @see #isSetLineId()
	 * @see #unsetLineId()
	 * @see #setLineId(int)
	 * @see org.telehash.model.TelehashPackage#getLine_LineId()
	 * @model unsettable="true"
	 * @generated
	 */
	int getLineId();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getLineId <em>Line Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Line Id</em>' attribute.
	 * @see #isSetLineId()
	 * @see #unsetLineId()
	 * @see #getLineId()
	 * @generated
	 */
	Line withLineId(int value);

	void setLineId(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getLineId <em>Line Id</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetLineId()
	 * @see #getLineId()
	 * @see #setLineId(int)
	 * @generated
	 */
	void unsetLineId();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getLineId <em>Line Id</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Line Id</em>' attribute is set.
	 * @see #unsetLineId()
	 * @see #getLineId()
	 * @see #setLineId(int)
	 * @generated
	 */
	boolean isSetLineId();

	/**
	 * Returns the value of the '<em><b>Visible</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Visible</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Visible</em>' attribute.
	 * @see #setVisible(boolean)
	 * @see org.telehash.model.TelehashPackage#getLine_Visible()
	 * @model default="false"
	 * @generated
	 */
	boolean isVisible();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#isVisible <em>Visible</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Visible</em>' attribute.
	 * @see #isVisible()
	 * @generated
	 */
	Line withVisible(boolean value);

	void setVisible(boolean value);

	/**
	 * Returns the value of the '<em><b>Advertised</b></em>' attribute.
	 * The default value is <code>"false"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Advertised</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Advertised</em>' attribute.
	 * @see #setAdvertised(boolean)
	 * @see org.telehash.model.TelehashPackage#getLine_Advertised()
	 * @model default="false"
	 * @generated
	 */
	boolean isAdvertised();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#isAdvertised <em>Advertised</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Advertised</em>' attribute.
	 * @see #isAdvertised()
	 * @generated
	 */
	Line withAdvertised(boolean value);

	void setAdvertised(boolean value);

	/**
	 * Returns the value of the '<em><b>Rules</b></em>' containment reference list.
	 * The list contents are of type {@link org.telehash.model.TapRule}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Rules</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Rules</em>' containment reference list.
	 * @see #isSetRules()
	 * @see #unsetRules()
	 * @see org.telehash.model.TelehashPackage#getLine_Rules()
	 * @model containment="true" unsettable="true"
	 * @generated
	 */
	EList<TapRule> getRules();

	/**
	 * Unsets the value of the '{@link org.telehash.model.Line#getRules <em>Rules</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRules()
	 * @see #getRules()
	 * @generated
	 */
	void unsetRules();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Line#getRules <em>Rules</em>}' containment reference list is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Rules</em>' containment reference list is set.
	 * @see #unsetRules()
	 * @see #getRules()
	 * @generated
	 */
	boolean isSetRules();

	/**
	 * Returns the value of the '<em><b>Session</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Session</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Session</em>' attribute.
	 * @see #setSession(IoSession)
	 * @see org.telehash.model.TelehashPackage#getLine_Session()
	 * @model dataType="org.telehash.model.IoSession" transient="true"
	 * @generated
	 */
	IoSession getSession();

	/**
	 * Sets the value of the '{@link org.telehash.model.Line#getSession <em>Session</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Session</em>' attribute.
	 * @see #getSession()
	 * @generated
	 */
	Line withSession(IoSession value);

	void setSession(IoSession value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @model
	 * @generated
	 */
	boolean isRulesMatch(Telex telex);

} // Line
