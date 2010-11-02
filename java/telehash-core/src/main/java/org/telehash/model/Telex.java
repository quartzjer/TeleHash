/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model;

import java.net.InetSocketAddress;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.json.model.JsObject;
import org.telehash.Hash;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Telex</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link org.telehash.model.Telex#getTo <em>To</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getEnd <em>End</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getLine <em>Line</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getRing <em>Ring</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getSee <em>See</em>}</li>
 *   <li>{@link org.telehash.model.Telex#getTap <em>Tap</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.telehash.model.TelehashPackage#getTelex()
 * @model
 * @generated
 */
public interface Telex extends JsObject {
	/**
	 * Returns the value of the '<em><b>To</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>To</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>To</em>' attribute.
	 * @see #setTo(InetSocketAddress)
	 * @see org.telehash.model.TelehashPackage#getTelex_To()
	 * @model dataType="org.telehash.model.Endpoint"
	 *        annotation="JsonMetadata keyType='header'"
	 * @generated
	 */
	InetSocketAddress getTo();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getTo <em>To</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>To</em>' attribute.
	 * @see #getTo()
	 * @generated
	 */
	Telex withTo(InetSocketAddress value);

	Telex withTo(Line line);

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getTo <em>To</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>To</em>' attribute.
	 * @see #getTo()
	 * @generated
	 */
	void setTo(InetSocketAddress value);

	/**
	 * Returns the value of the '<em><b>End</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>End</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>End</em>' attribute.
	 * @see #isSetEnd()
	 * @see #unsetEnd()
	 * @see #setEnd(Hash)
	 * @see org.telehash.model.TelehashPackage#getTelex_End()
	 * @model unsettable="true" dataType="org.telehash.model.Hash"
	 *        annotation="JsonMetadata keyType='signal'"
	 * @generated
	 */
	Hash getEnd();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getEnd <em>End</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>End</em>' attribute.
	 * @see #isSetEnd()
	 * @see #unsetEnd()
	 * @see #getEnd()
	 * @generated
	 */
	Telex withEnd(Hash value);

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getEnd <em>End</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>End</em>' attribute.
	 * @see #getEnd()
	 * @generated
	 */
	void setEnd(Hash value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Telex#getEnd <em>End</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetEnd()
	 * @see #getEnd()
	 * @see #setEnd(Hash)
	 * @generated
	 */
	void unsetEnd();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Telex#getEnd <em>End</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>End</em>' attribute is set.
	 * @see #unsetEnd()
	 * @see #getEnd()
	 * @see #setEnd(Hash)
	 * @generated
	 */
	boolean isSetEnd();

	/**
	 * Returns the value of the '<em><b>Line</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Line</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Line</em>' attribute.
	 * @see #isSetLine()
	 * @see #unsetLine()
	 * @see #setLine(int)
	 * @see org.telehash.model.TelehashPackage#getTelex_Line()
	 * @model unsettable="true"
	 *        annotation="JsonMetadata keyType='header'"
	 * @generated
	 */
	int getLine();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getLine <em>Line</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Line</em>' attribute.
	 * @see #isSetLine()
	 * @see #unsetLine()
	 * @see #getLine()
	 * @generated
	 */
	Telex withLine(int value);

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getLine <em>Line</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Line</em>' attribute.
	 * @see #isSetLine()
	 * @see #unsetLine()
	 * @see #getLine()
	 * @generated
	 */
	void setLine(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Telex#getLine <em>Line</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetLine()
	 * @see #getLine()
	 * @see #setLine(int)
	 * @generated
	 */
	void unsetLine();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Telex#getLine <em>Line</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Line</em>' attribute is set.
	 * @see #unsetLine()
	 * @see #getLine()
	 * @see #setLine(int)
	 * @generated
	 */
	boolean isSetLine();

	/**
	 * Returns the value of the '<em><b>Ring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Ring</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Ring</em>' attribute.
	 * @see #isSetRing()
	 * @see #unsetRing()
	 * @see #setRing(int)
	 * @see org.telehash.model.TelehashPackage#getTelex_Ring()
	 * @model unsettable="true"
	 *        annotation="JsonMetadata keyType='header'"
	 * @generated
	 */
	int getRing();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getRing <em>Ring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ring</em>' attribute.
	 * @see #isSetRing()
	 * @see #unsetRing()
	 * @see #getRing()
	 * @generated
	 */
	Telex withRing(int value);

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getRing <em>Ring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Ring</em>' attribute.
	 * @see #isSetRing()
	 * @see #unsetRing()
	 * @see #getRing()
	 * @generated
	 */
	void setRing(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Telex#getRing <em>Ring</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetRing()
	 * @see #getRing()
	 * @see #setRing(int)
	 * @generated
	 */
	void unsetRing();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Telex#getRing <em>Ring</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Ring</em>' attribute is set.
	 * @see #unsetRing()
	 * @see #getRing()
	 * @see #setRing(int)
	 * @generated
	 */
	boolean isSetRing();

	/**
	 * Returns the value of the '<em><b>See</b></em>' attribute list.
	 * The list contents are of type {@link java.net.InetSocketAddress}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>See</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>See</em>' attribute list.
	 * @see #isSetSee()
	 * @see #unsetSee()
	 * @see org.telehash.model.TelehashPackage#getTelex_See()
	 * @model unsettable="true" dataType="org.telehash.model.Endpoint"
	 *        annotation="JsonMetadata keyType='command'"
	 * @generated
	 */
	EList<InetSocketAddress> getSee();

	/**
	 * Unsets the value of the '{@link org.telehash.model.Telex#getSee <em>See</em>}' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetSee()
	 * @see #getSee()
	 * @generated
	 */
	void unsetSee();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Telex#getSee <em>See</em>}' attribute list is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>See</em>' attribute list is set.
	 * @see #unsetSee()
	 * @see #getSee()
	 * @generated
	 */
	boolean isSetSee();

	/**
	 * Returns the value of the '<em><b>Tap</b></em>' containment reference list.
	 * The list contents are of type {@link org.telehash.model.TapRule}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Tap</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Tap</em>' containment reference list.
	 * @see #isSetTap()
	 * @see #unsetTap()
	 * @see org.telehash.model.TelehashPackage#getTelex_Tap()
	 * @model containment="true" unsettable="true"
	 *        annotation="JsonMetadata keyType='command'"
	 * @generated
	 */
	EList<TapRule> getTap();

	/**
	 * Unsets the value of the '{@link org.telehash.model.Telex#getTap <em>Tap</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetTap()
	 * @see #getTap()
	 * @generated
	 */
	void unsetTap();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Telex#getTap <em>Tap</em>}' containment reference list is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Tap</em>' containment reference list is set.
	 * @see #unsetTap()
	 * @see #getTap()
	 * @generated
	 */
	boolean isSetTap();

	/**
	 * Returns the value of the '<em><b>Bytes Received</b></em>' attribute.
	 * The default value is <code>"0"</code>.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Bytes Received</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Bytes Received</em>' attribute.
	 * @see #isSetBytesReceived()
	 * @see #unsetBytesReceived()
	 * @see #setBytesReceived(int)
	 * @see org.telehash.model.TelehashPackage#getTelex_BytesReceived()
	 * @model default="0" unsettable="true"
	 *        annotation="JsonMetadata keyType='header' key='br'"
	 * @generated
	 */
	int getBytesReceived();

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Bytes Received</em>' attribute.
	 * @see #isSetBytesReceived()
	 * @see #unsetBytesReceived()
	 * @see #getBytesReceived()
	 * @generated
	 */
	Telex withBytesReceived(int value);

	/**
	 * Sets the value of the '{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Bytes Received</em>' attribute.
	 * @see #getBytesReceived()
	 * @generated
	 */
	void setBytesReceived(int value);

	/**
	 * Unsets the value of the '{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isSetBytesReceived()
	 * @see #getBytesReceived()
	 * @see #setBytesReceived(int)
	 * @generated
	 */
	void unsetBytesReceived();

	/**
	 * Returns whether the value of the '{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}' attribute is set.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return whether the value of the '<em>Bytes Received</em>' attribute is set.
	 * @see #unsetBytesReceived()
	 * @see #getBytesReceived()
	 * @see #setBytesReceived(int)
	 * @generated
	 */
	boolean isSetBytesReceived();

} // Telex
